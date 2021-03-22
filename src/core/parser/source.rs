use crate::errors::MosResult;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Read;
use std::path::Path;
use std::sync::Arc;

/// A source of data for the parser. Maps paths to their contents.
pub trait ParsingSource {
    fn get_contents(&self, path: &Path) -> MosResult<String>;
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

impl From<FileSystemParsingSource> for Arc<RefCell<dyn ParsingSource>> {
    fn from(t: FileSystemParsingSource) -> Self {
        Arc::new(RefCell::new(t))
    }
}

impl ParsingSource for FileSystemParsingSource {
    fn get_contents(&self, path: &Path) -> MosResult<String> {
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

impl From<InMemoryParsingSource> for Arc<RefCell<dyn ParsingSource>> {
    fn from(t: InMemoryParsingSource) -> Self {
        Arc::new(RefCell::new(t))
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
    fn get_contents(&self, path: &Path) -> MosResult<String> {
        Ok(self.files.get(path.to_str().unwrap()).unwrap().to_string())
    }
}
