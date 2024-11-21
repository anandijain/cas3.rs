use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

pub type Result<T> = std::result::Result<T, Cas3Error>;

#[derive(Debug)]
pub struct Cas3Error {
    kind: Box<Cas3ErrorKind>,
}

#[derive(Debug)]
pub enum Cas3ErrorKind {
    IoError(std::io::Error)
}

impl Error for Cas3Error {}

impl Display for Cas3Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.kind.as_ref(), f)
    }
}

impl Display for Cas3ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Cas3ErrorKind::IoError(v) => {
                write!(f, "IO Error: {}", v)
            }
        }
    }
}

impl From<std::io::Error> for Cas3Error {
    fn from(value: std::io::Error) -> Self {
        Self { kind: Box::new(Cas3ErrorKind::IoError(value)) }
    }
}


