use crate::errors::{CoreError, CoreResult};
use std::collections::HashMap;
use std::io::{ErrorKind, Read};
use std::path::Path;
use std::sync::{Arc, Mutex};

/// A source of data for the parser. Maps paths to their contents.
pub trait ParsingSource {
    fn get_contents(&self, path: &Path) -> CoreResult<String>;

    fn try_get_contents(&self, path: &Path) -> Option<String> {
        self.get_contents(path).ok()
    }

    fn exists(&self, path: &Path) -> bool {
        self.try_get_contents(path).is_some()
    }
}

pub struct FileSystemParsingSource {}

impl FileSystemParsingSource {
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for FileSystemParsingSource {
    fn default() -> Self {
        Self::new()
    }
}

impl From<FileSystemParsingSource> for Arc<Mutex<dyn ParsingSource>> {
    fn from(t: FileSystemParsingSource) -> Self {
        Arc::new(Mutex::new(t))
    }
}

impl ParsingSource for FileSystemParsingSource {
    fn get_contents(&self, path: &Path) -> CoreResult<String> {
        let mut file = fs_err::File::open(&path)?;
        let mut source = String::new();
        file.read_to_string(&mut source)?;
        Ok(source)
    }
}

pub struct InMemoryParsingSource {
    files: HashMap<String, String>,
}

impl Default for InMemoryParsingSource {
    fn default() -> Self {
        Self::new()
    }
}

impl From<InMemoryParsingSource> for Arc<Mutex<dyn ParsingSource>> {
    fn from(t: InMemoryParsingSource) -> Self {
        Arc::new(Mutex::new(t))
    }
}

impl InMemoryParsingSource {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
    }

    pub fn add<F: Into<String>>(mut self, filename: F, src: &str) -> Self {
        let filename = filename.into();
        self.files.insert(filename, src.into());
        self
    }
}

impl ParsingSource for InMemoryParsingSource {
    fn get_contents(&self, path: &Path) -> CoreResult<String> {
        match self.files.get(path.to_str().unwrap()) {
            Some(data) => Ok(data.to_string()),
            None => Err(CoreError::Io(std::io::Error::new(
                ErrorKind::NotFound,
                path.to_str().unwrap(),
            ))),
        }
    }
}
