use thiserror::Error;

pub type Answer = anyhow::Result<i64>;
pub type Solution = fn(&str) -> anyhow::Result<(Answer, Answer)>;

#[derive(Debug, Error)]
pub struct NoSolutionError;

impl std::fmt::Display for NoSolutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "no solution exists.")
    }
}

#[derive(Debug, Error)]
pub struct BadInputError(pub String);

impl std::fmt::Display for BadInputError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bad input: \'{}\' .", self.0)
    }
}
