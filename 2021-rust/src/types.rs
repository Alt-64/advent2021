use std::{
    fmt::{Debug, Display},
    sync::{mpsc::Sender, Arc},
};

use thiserror::Error;

pub type Solution = anyhow::Result<Box<dyn Display + Send + Sync>>;

pub type Solver = fn(input: &str, tx: Sender<(usize, usize, Solution)>) -> anyhow::Result<()>;

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

// pub fn expect_soln<T: Display + Send + Sync>(
//     x: Option<T>,
// ) -> anyhow::Result<Box<dyn Display + Send + Sync>> {
//     x.map(|x| Box::new(x) as Box<dyn Display + Send + Sync>)
//         .ok_or(NoSolutionError)
//         .map_err(Into::into)
// }
