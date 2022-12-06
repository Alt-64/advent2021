use anyhow::Result;
use std::{
    fmt::{Debug, Display},
    num::ParseIntError,
    sync,
};

use thiserror::Error;

pub type Day = usize;
pub type Part = usize;
pub type SolutionSender<T: Display + Send + Sync> = sync::mpsc::Sender<(Day, Part, Result<T>)>;

pub trait Solver: for<'a> TryFrom<&'a str> + Sized {
    type Soln1: Display + Send + Sync;
    type Soln2: Display + Send + Sync;
    fn solve_part1(&self) -> Result<Self::Soln1>;
    fn solve_part2(&self) -> Result<Self::Soln2>;
    
    fn solve(input: &str, tx1: Sender<) {}
}

#[derive(Debug, Error)]
pub struct NoSolutionError;

impl std::fmt::Display for NoSolutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "no solution exists.")
    }
}

#[derive(Debug, Error)]
pub struct BadInputError(pub String);

impl From<ParseIntError> for BadInputError {
    fn from(e: ParseIntError) -> Self {
        BadInputError(e.to_string())
    }
}

impl std::fmt::Display for BadInputError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bad input: \'{}\' .", self.0)
    }
}
