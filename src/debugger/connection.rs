use crate::debugger::protocol::ProtocolMessage;
use crate::errors::MosResult;
use crossbeam_channel::{bounded, Receiver, Sender};
use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::{io, thread};

pub struct DebugConnection {
    pub sender: Sender<ProtocolMessage>,
    pub receiver: Receiver<ProtocolMessage>,
}

impl DebugConnection {
    pub fn tcp(address: &str) -> MosResult<(DebugConnection, DebugIoThreads)> {
        let listener = TcpListener::bind(address)?;
        let (stream, _) = listener.accept()?;
        let (reader_receiver, reader) = make_reader(stream.try_clone().unwrap());
        let (writer_sender, writer) = make_write(stream.try_clone().unwrap());
        let io_threads = DebugIoThreads { reader, writer };
        Ok((
            DebugConnection {
                sender: writer_sender,
                receiver: reader_receiver,
            },
            io_threads,
        ))
    }
}

#[allow(dead_code)]
pub struct DebugIoThreads {
    reader: thread::JoinHandle<io::Result<()>>,
    writer: thread::JoinHandle<io::Result<()>>,
}

impl ProtocolMessage {
    pub fn read(r: &mut impl BufRead) -> io::Result<Option<ProtocolMessage>> {
        ProtocolMessage::_read(r)
    }
    fn _read(r: &mut dyn BufRead) -> io::Result<Option<ProtocolMessage>> {
        let text = match read_msg_text(r)? {
            None => return Ok(None),
            Some(text) => text,
        };
        let msg = serde_json::from_str(&text)?;
        Ok(Some(msg))
    }
    pub fn write(self, w: &mut impl Write) -> io::Result<()> {
        let text = serde_json::to_string(&self)?;
        write_msg_text(w, &text)
    }
}

fn read_msg_text(inp: &mut dyn BufRead) -> io::Result<Option<String>> {
    fn invalid_data(error: impl Into<Box<dyn std::error::Error + Send + Sync>>) -> io::Error {
        io::Error::new(io::ErrorKind::InvalidData, error)
    }
    macro_rules! invalid_data {
        ($($tt:tt)*) => (invalid_data(format!($($tt)*)))
    }

    let mut size = None;
    let mut buf = String::new();
    loop {
        buf.clear();
        if inp.read_line(&mut buf)? == 0 {
            return Ok(None);
        }
        if !buf.ends_with("\r\n") {
            return Err(invalid_data!("malformed header: {:?}", buf));
        }
        let buf = &buf[..buf.len() - 2];
        if buf.is_empty() {
            break;
        }
        let mut parts = buf.splitn(2, ": ");
        let header_name = parts.next().unwrap();
        let header_value = parts
            .next()
            .ok_or_else(|| invalid_data!("malformed header: {:?}", buf))?;
        if header_name == "Content-Length" {
            size = Some(header_value.parse::<usize>().map_err(invalid_data)?);
        }
    }
    let size: usize = size.ok_or_else(|| invalid_data!("no Content-Length"))?;
    let mut buf = buf.into_bytes();
    buf.resize(size, 0);
    inp.read_exact(&mut buf)?;
    let buf = String::from_utf8(buf).map_err(invalid_data)?;
    log::debug!("< {}", buf);
    Ok(Some(buf))
}

fn write_msg_text(out: &mut dyn Write, msg: &str) -> io::Result<()> {
    log::debug!("> {}", msg);
    write!(out, "Content-Length: {}\r\n\r\n", msg.len())?;
    out.write_all(msg.as_bytes())?;
    out.flush()?;
    Ok(())
}

fn make_reader(
    stream: TcpStream,
) -> (
    Receiver<ProtocolMessage>,
    thread::JoinHandle<io::Result<()>>,
) {
    let (reader_sender, reader_receiver) = bounded::<ProtocolMessage>(0);
    let reader = thread::spawn(move || {
        let mut buf_read = BufReader::new(stream);
        while let Some(msg) = ProtocolMessage::read(&mut buf_read).unwrap() {
            let is_exit = match &msg {
                ProtocolMessage::Request(req) => req.command == "disconnect",
                _ => false,
            };
            reader_sender.send(msg).unwrap();
            if is_exit {
                break;
            }
        }
        Ok(())
    });
    (reader_receiver, reader)
}

fn make_write(
    mut stream: TcpStream,
) -> (Sender<ProtocolMessage>, thread::JoinHandle<io::Result<()>>) {
    let (writer_sender, writer_receiver) = bounded::<ProtocolMessage>(0);
    let writer = thread::spawn(move || {
        writer_receiver
            .into_iter()
            .try_for_each(|it| it.write(&mut stream))
            .unwrap();
        Ok(())
    });
    (writer_sender, writer)
}
