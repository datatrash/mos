#![allow(dead_code)]
use crate::errors::{MosError, MosResult};
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use crossbeam_channel::{bounded, Receiver, Sender};
use std::collections::HashMap;
use std::io::{BufRead, BufReader, ErrorKind, Write};
use std::net::TcpStream;
use std::process::{Child, Command};
use std::time::Duration;
use std::{io, thread};

pub struct ViceAdapter {
    process: Option<Child>,
    connection: Option<ViceConnection>,
}

struct ViceConnection {
    sender: Sender<ViceRequest>,
    receiver: Receiver<ViceResponse>,
}

impl ViceAdapter {
    pub fn new() -> Self {
        Self {
            process: None,
            connection: None,
        }
    }

    pub fn start(&mut self, vice_path: &str, args: Vec<&str>) -> MosResult<()> {
        self.process = Some(Command::new(&vice_path).args(&args).spawn()?);

        let stream = loop {
            let stream = TcpStream::connect_timeout(
                &"127.0.0.1:6502".parse().unwrap(),
                Duration::from_secs(1),
            );
            match stream {
                Ok(s) => break s,
                Err(e) if e.kind() == ErrorKind::ConnectionRefused => {
                    log::debug!("VICE refused connection...");
                    thread::sleep(Duration::from_millis(100));
                }
                Err(e) => {
                    let m: MosError = e.into();
                    return Err(m);
                }
            }
        };
        log::debug!("VICE is connected.");
        let (reader_receiver, _reader) = make_reader(stream.try_clone().unwrap());
        let (writer_sender, _writer) = make_writer(stream.try_clone().unwrap());

        self.connection = Some(ViceConnection {
            receiver: reader_receiver,
            sender: writer_sender,
        });

        Ok(())
    }

    pub fn stop(&mut self) -> MosResult<()> {
        self.send(ViceRequest::Quit)?;
        self.process = None;
        self.connection = None;

        Ok(())
    }

    pub fn send(&self, msg: ViceRequest) -> MosResult<()> {
        if let Some(c) = &self.connection {
            c.sender.send(msg)?;
        }
        Ok(())
    }

    pub fn receiver(&self) -> Option<Receiver<ViceResponse>> {
        self.connection.as_ref().map(|c| c.receiver.clone())
    }
}

#[derive(Debug)]
pub enum ViceResponse {
    AdvanceInstructions,
    CheckpointDelete,
    CheckpointResponse(CheckpointResponse),
    CheckpointList(u32),
    CheckpointToggle,
    Exit,
    ExecuteUntilReturn,
    Ping,
    Quit,
    Stopped(u16),
    Registers(HashMap<u8, u16>),
    RegistersAvailable(HashMap<u8, String>),
    Resumed(u16),
}

#[derive(Debug)]
pub struct CheckpointResponse {
    pub number: u32,
    pub currently_hit: bool,
    pub start: u16,
    pub end: u16,
    pub stop_when_hit: bool,
    pub enabled: bool,
    pub cpu_operation: u8,
    pub temporary: bool,
    pub hit_count: u32,
    pub ignore_count: u32,
    pub has_condition: bool,
}

#[derive(Debug)]
pub enum ViceRequest {
    AdvanceInstructions(bool, u16),
    CheckpointList,
    CheckpointDelete(u32),
    CheckpointSet(CheckpointSet),
    CheckpointToggle(u32, bool),
    ExecuteUntilReturn,
    Exit,
    Ping,
    RegistersGet,
    RegistersAvailable,
    Quit,
}

#[derive(Debug)]
pub struct CheckpointSet {
    pub start: u16,
    pub end: u16,
    pub stop_when_hit: bool,
    pub enabled: bool,
    pub cpu_operation: u8,
    pub temporary: bool,
}

struct ViceRequestHeader {
    stx: u8,
    api_version: u8,
    length: u32,
    request_id: u32,
    command_type: u8,
}

struct ViceResponseHeader {
    stx: u8,
    api_version: u8,
    length: u32,
    response_type: u8,
    error_code: u8,
    request_id: u32,
}

impl ViceResponse {
    fn read(input: &mut dyn BufRead) -> MosResult<ViceResponse> {
        let header = ViceResponseHeader {
            stx: input.read_u8()?,
            api_version: input.read_u8()?,
            length: input.read_u32::<LittleEndian>()?,
            response_type: input.read_u8()?,
            error_code: input.read_u8()?,
            request_id: input.read_u32::<LittleEndian>()?,
        };
        assert_eq!(header.stx, 2);
        assert_eq!(header.api_version, 1);

        let response = match header.response_type {
            0x11 => ViceResponse::CheckpointResponse(CheckpointResponse {
                number: input.read_u32::<LittleEndian>()?,
                currently_hit: input.read_u8()? != 0,
                start: input.read_u16::<LittleEndian>()?,
                end: input.read_u16::<LittleEndian>()?,
                stop_when_hit: input.read_u8()? != 0,
                enabled: input.read_u8()? != 0,
                cpu_operation: input.read_u8()?,
                temporary: input.read_u8()? != 0,
                hit_count: input.read_u32::<LittleEndian>()?,
                ignore_count: input.read_u32::<LittleEndian>()?,
                has_condition: input.read_u8()? != 0,
            }),
            0x13 => ViceResponse::CheckpointDelete,
            0x14 => ViceResponse::CheckpointList(input.read_u32::<LittleEndian>()?),
            0x15 => ViceResponse::CheckpointToggle,
            0x31 => {
                let mut regs = HashMap::new();
                let len = input.read_u16::<LittleEndian>()?;
                for _ in 0..len {
                    let _size = input.read_u8()?;
                    let id = input.read_u8()?;
                    let value = input.read_u16::<LittleEndian>()?;
                    regs.insert(id, value);
                }
                ViceResponse::Registers(regs)
            }
            0x62 => ViceResponse::Stopped(input.read_u16::<LittleEndian>()?),
            0x63 => ViceResponse::Resumed(input.read_u16::<LittleEndian>()?),
            0x71 => ViceResponse::AdvanceInstructions,
            0x73 => ViceResponse::ExecuteUntilReturn,
            0x81 => ViceResponse::Ping,
            0x83 => {
                let mut regs = HashMap::new();
                let len = input.read_u16::<LittleEndian>()?;
                for _ in 0..len {
                    let _size = input.read_u8()?;
                    let id = input.read_u8()?;
                    let _reg_size = input.read_u8()?;
                    let name_length = input.read_u8()?;
                    let mut name_buf = vec![0u8; name_length as usize];
                    input.read_exact(&mut name_buf)?;
                    let name = String::from_utf8(name_buf).expect("Not a valid register name");
                    regs.insert(id, name);
                }
                ViceResponse::RegistersAvailable(regs)
            }
            0xaa => ViceResponse::Exit,
            0xbb => ViceResponse::Quit,
            _ => panic!("Unknown response type: 0x{:0X}", header.response_type),
        };
        Ok(response)
    }
}

fn bool_to_u8(val: bool) -> u8 {
    if val {
        1
    } else {
        0
    }
}

impl ViceRequest {
    fn write(self, w: &mut impl Write) -> io::Result<()> {
        let (command_type, body): (u8, Vec<u8>) = match &self {
            ViceRequest::AdvanceInstructions(step_over_subroutines, instructions_to_skip) => {
                let mut body = vec![];
                body.write_u8(bool_to_u8(*step_over_subroutines))?;
                body.write_u16::<LittleEndian>(*instructions_to_skip)?;
                (0x71, body)
            }
            ViceRequest::CheckpointSet(c) => {
                let mut body = vec![];
                body.write_u16::<LittleEndian>(c.start)?;
                body.write_u16::<LittleEndian>(c.end)?;
                body.write_u8(bool_to_u8(c.stop_when_hit))?;
                body.write_u8(bool_to_u8(c.enabled))?;
                body.write_u8(c.cpu_operation)?;
                body.write_u8(bool_to_u8(c.temporary))?;
                (0x12, body)
            }
            ViceRequest::CheckpointDelete(number) => {
                let mut body = vec![];
                body.write_u32::<LittleEndian>(*number)?;
                (0x13, body)
            }
            ViceRequest::CheckpointList => (0x14, vec![]),
            ViceRequest::CheckpointToggle(number, enabled) => {
                let mut body = vec![];
                body.write_u32::<LittleEndian>(*number)?;
                body.write_u8(bool_to_u8(*enabled))?;
                (0x15, body)
            }
            ViceRequest::RegistersGet => (0x31, vec![0]),
            ViceRequest::ExecuteUntilReturn => (0x73, vec![]),
            ViceRequest::Ping => (0x81, vec![]),
            ViceRequest::RegistersAvailable => (0x83, vec![0]),
            ViceRequest::Exit => (0xaa, vec![]),
            ViceRequest::Quit => (0xbb, vec![]),
        };
        let mut out = vec![];
        out.write_u8(2)?; // stx
        out.write_u8(1)?; // api version
        out.write_u32::<LittleEndian>(body.len() as u32)?; // body length
        out.write_u32::<LittleEndian>(1)?; // request id
        out.write_u8(command_type)?;
        w.write_all(&out)?;
        w.write_all(&body)?;
        w.flush()?;
        Ok(())
    }
}

fn make_reader(stream: TcpStream) -> (Receiver<ViceResponse>, thread::JoinHandle<io::Result<()>>) {
    let (reader_sender, reader_receiver) = bounded::<ViceResponse>(0);
    let reader = thread::spawn(move || {
        let mut buf_read = BufReader::new(stream);
        while let Ok(msg) = ViceResponse::read(&mut buf_read) {
            let is_exit = reader_sender.send(msg).is_err();
            if is_exit {
                break;
            }
        }
        Ok(())
    });
    (reader_receiver, reader)
}

fn make_writer(mut stream: TcpStream) -> (Sender<ViceRequest>, thread::JoinHandle<io::Result<()>>) {
    let (writer_sender, writer_receiver) = bounded::<ViceRequest>(0);
    let writer = thread::spawn(move || {
        writer_receiver
            .into_iter()
            .try_for_each(|it| it.write(&mut stream))
            .unwrap();
        Ok(())
    });
    (writer_sender, writer)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::errors::MosResult;
    use crate::testing::enable_default_tracing;

    #[ignore]
    #[test]
    fn run_vice() -> MosResult<()> {
        enable_default_tracing();

        #[cfg(not(target_os = "macos"))]
        let vice_path = "...unsupported on this OS...";

        #[cfg(target_os = "macos")]
        let vice_path = "/Applications/vice-sdl2-3.5/x64sc.app/Contents/MacOS/x64sc";

        let mut vice = ViceAdapter::new();
        vice.start(vice_path, vec!["-binarymonitor"])?;
        vice.send(ViceRequest::RegistersAvailable)?;
        vice.send(ViceRequest::Ping)?;
        vice.send(ViceRequest::Quit)?;

        for msg in &vice.receiver() {
            log::info!("msg: {:?}", msg);
        }
        vice.stop()?;

        Ok(())
    }
}
