use std::{
    fmt::{Debug, Display},
    sync::mpsc::Sender,
};

use thiserror::Error;

pub(crate) trait Solver<'a>: TryFrom<&'a str> + Iterator<Item = Box<dyn Debug>> {
    type Soln1: Debug;
    type Soln2: Debug;
    fn solve_part1(&mut self) -> Self::Soln1;
    fn solve_part2(&mut self) -> Self::Soln2;
}

pub(crate) struct SolveState(usize);

impl Iterator for SolveState {
    type Item = Box<dyn Debug>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0 += 1;
        match self.0 {
            1 => Some(Box::new(self.solve_part1())),
            2 => Some(Box::new(self.solve_part2())),
            _ => None,
        }
    }
}

// type Solution = anyhow::Result<Box<dyn Display>>;
// type ProblemState = (Option<Solution>, Option<Solution>);
//
// type Solver = fn(input: &str, Sender<usize, ProblemState>) -> Result<(), ()>;

impl SolveState {
    pub fn new() -> Self {
        SolveState(0)
    }
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

impl std::fmt::Display for BadInputError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bad input: \'{}\' .", self.0)
    }
}
