pub type Solution = Result<i32, Error>;

#[derive(Debug)]
pub enum Error {
    IOError(std::io::Error),
    ParseIntError(std::num::ParseIntError),
    Unrecognized(String),
    Malformed(String),
    Example,
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IOError(e)
    }
}
impl From<std::num::ParseIntError> for Error {
    fn from(e: std::num::ParseIntError) -> Self {
        Error::ParseIntError(e)
    }
}
