#[derive(Debug)]
pub enum Error {
    IOError(std::io::Error),
    StringParseError(StringParseError),
    Example,
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IOError(e)
    }
}
impl From<StringParseError> for Error {
    fn from(e: StringParseError) -> Self {
        Error::StringParseError(e)
    }
}

#[derive(Debug)]
pub enum StringParseError {
    NoValue(std::num::ParseIntError),
    Unrecognized(String),
    Malformed(String),
}

impl From<std::num::ParseIntError> for StringParseError {
    fn from(e: std::num::ParseIntError) -> Self {
        StringParseError::NoValue(e)
    }
}
