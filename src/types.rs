pub type Solution = Result<i64, Error>;

#[derive(Debug)]
pub enum Error {
    IOError(std::io::Error),
    ParseIntError(std::num::ParseIntError),
    ParseFloatError(std::num::ParseFloatError),
    TryFromVecError,
    TryFromIntError(std::num::TryFromIntError),
    BadInput(String),
    NoSolution,
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
impl From<std::num::ParseFloatError> for Error {
    fn from(e: std::num::ParseFloatError) -> Self {
        Error::ParseFloatError(e)
    }
}
impl From<std::num::TryFromIntError> for Error {
    fn from(e: std::num::TryFromIntError) -> Self {
        Error::TryFromIntError(e)
    }
}
impl<T> From<Vec<T>> for Error {
    fn from(_: Vec<T>) -> Self {
        Error::TryFromVecError
    }
}
impl FromIterator<std::num::ParseIntError> for Error {
    fn from_iter<I: IntoIterator<Item = std::num::ParseIntError>>(_: I) -> Self {
        Error::TryFromVecError
    }
}
