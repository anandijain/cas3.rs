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

impl Display for Cas3ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Cas3ErrorKind::IoError(v) => {
                write!(f, "IO Error: {}", v)
            }
        }
    }
}


impl Display for Cas3Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Error for Cas3Error {}