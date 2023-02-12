// https://adventofcode.com/2021/day/1
use anyhow::Result;
use std::num::ParseIntError;

use crate::types::{SolveState, Solver};
use std::fmt::Debug;

pub struct Day1 {
    state: SolveState,
    depths: Vec<i64>,
}

impl TryFrom<&str> for Day1 {
    type Error = ParseIntError;
    fn try_from(input: &str) -> Result<Day1, ParseIntError> {
        Ok(Day1 {
            state: SolveState::new(),
            depths: input
                .split("\n")
                .map(str::parse)
                .collect::<Result<_, _>>()?,
        })
    }
}

impl Solver<'_> for Day1 {
    type Soln1 = usize;
    fn solve_part1(&mut self) -> Self::Soln1 {
        self.depths
            .array_windows::<2>()
            .filter(|&[left, right]| left < right)
            .count()
    }

    type Soln2 = usize;
    fn solve_part2(&mut self) -> Self::Soln2 {
        self.depths
            .array_windows::<4>()
            .filter(|&[left, _, _, right]| left < right)
            .count()
    }
}

impl Iterator for Day1 {
    type Item = Box<dyn Debug>;

    fn next(&mut self) -> Option<Self::Item> {
        self.state.next()
    }
}
