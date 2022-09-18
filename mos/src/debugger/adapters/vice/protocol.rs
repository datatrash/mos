use crate::diagnostic_emitter::MosResult;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use std::collections::HashMap;
use std::io;
use std::io::{BufRead, Write};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ViceResponse {
    AdvanceInstructions,
    BanksAvailable(HashMap<u16, String>),
    CheckpointDelete,
    CheckpointResponse(CheckpointResponse),
    CheckpointList(u32),
    CheckpointToggle,
    Exit,
    ExecuteUntilReturn,
    MemoryGet(Vec<u8>),
    MemorySet,
    Ping,
    Quit,
    Stopped(u16),
    Registers(HashMap<u8, u16>),
    RegistersAvailable(HashMap<u8, String>),
    Resumed(u16),
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
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

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub enum ViceRequest {
    AdvanceInstructions(bool, u16),
    BanksAvailable,
    CheckpointList,
    CheckpointDelete(u32),
    CheckpointSet(CheckpointSet),
    CheckpointToggle(u32, bool),
    ExecuteUntilReturn,
    Exit,
    MemoryGet(MemoryDescriptor),
    MemorySet(MemoryDescriptor, Vec<u8>),
    Ping,
    RegistersGet,
    RegistersSet(u8, u8),
    RegistersAvailable,
    Quit,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CheckpointSet {
    pub start: u16,
    pub end: u16,
    pub stop_when_hit: bool,
    pub enabled: bool,
    pub cpu_operation: u8,
    pub temporary: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemoryDescriptor {
    pub cause_side_effects: bool,
    pub start: u16,
    pub end: u16,
    pub memory_space: u8,
    pub bank_id: u16,
}

impl MemoryDescriptor {
    fn get_buffer(&self) -> io::Result<Vec<u8>> {
        let mut body = vec![];
        body.write_u8(bool_to_u8(self.cause_side_effects))?;
        body.write_u16::<LittleEndian>(self.start)?;
        body.write_u16::<LittleEndian>(self.end)?;
        body.write_u8(self.memory_space)?;
        body.write_u16::<LittleEndian>(self.bank_id)?;
        Ok(body)
    }
}

struct ViceResponseHeader {
    stx: u8,
    api_version: u8,
    _length: u32,
    response_type: u8,
    _error_code: u8,
    _request_id: u32,
}

impl ViceResponse {
    pub fn read(input: &mut dyn BufRead) -> MosResult<ViceResponse> {
        let header = ViceResponseHeader {
            stx: input.read_u8()?,
            api_version: input.read_u8()?,
            _length: input.read_u32::<LittleEndian>()?,
            response_type: input.read_u8()?,
            _error_code: input.read_u8()?,
            _request_id: input.read_u32::<LittleEndian>()?,
        };
        assert_eq!(header.stx, 2);
        assert_eq!(header.api_version, 1);

        let response = match header.response_type {
            0x01 => {
                let len = input.read_u16::<LittleEndian>()?;
                let mut buf = vec![0u8; len as usize];
                input.read_exact(&mut buf)?;
                ViceResponse::MemoryGet(buf)
            }
            0x02 => ViceResponse::MemorySet,
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
            0x82 => {
                let mut banks = HashMap::new();
                let len = input.read_u16::<LittleEndian>()?;
                for _ in 0..len {
                    let _size = input.read_u8()?;
                    let id = input.read_u16::<LittleEndian>()?;
                    let name_length = input.read_u8()?;
                    let mut name_buf = vec![0u8; name_length as usize];
                    input.read_exact(&mut name_buf)?;
                    let name = String::from_utf8(name_buf).expect("Not a valid bank name");
                    banks.insert(id, name);
                }
                ViceResponse::BanksAvailable(banks)
            }
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
    pub fn write(self, w: &mut impl Write) -> io::Result<()> {
        let (command_type, body): (u8, Vec<u8>) = match &self {
            ViceRequest::MemoryGet(desc) => {
                let buf = desc.get_buffer()?;
                (0x01, buf)
            }
            ViceRequest::MemorySet(desc, data) => {
                let mut buf = desc.get_buffer()?;
                buf.append(&mut data.clone());
                (0x02, buf)
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
            ViceRequest::RegistersSet(id, value) => {
                let mut body = vec![];
                body.write_u8(0)?;
                body.write_u16::<LittleEndian>(1)?;
                body.write_u8(16)?;
                body.write_u8(*id)?;
                body.write_u16::<LittleEndian>(*value as u16)?;
                (0x32, body)
            }
            ViceRequest::AdvanceInstructions(step_over_subroutines, instructions_to_skip) => {
                let mut body = vec![];
                body.write_u8(bool_to_u8(*step_over_subroutines))?;
                body.write_u16::<LittleEndian>(*instructions_to_skip)?;
                (0x71, body)
            }
            ViceRequest::ExecuteUntilReturn => (0x73, vec![]),
            ViceRequest::Ping => (0x81, vec![]),
            ViceRequest::BanksAvailable => (0x82, vec![]),
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
