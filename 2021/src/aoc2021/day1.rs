// https://adventofcode.com/2021/day/1
use anyhow::Result;
use std::num::ParseIntError;

use crate::types::Solver;

pub struct Day1(Vec<i64>);

impl Solver for Day1 {
    type Soln1 = usize;
    fn solve_part1(&self) -> Result<Self::Soln1> {
        let dives = self
            .0
            .array_windows::<2>()
            .filter(|&[left, right]| left < right)
            .count();
        Ok(dives)
    }

    type Soln2 = usize;
    fn solve_part2(&self) -> Result<Self::Soln2> {
        let big_dives = self
            .0
            .array_windows::<4>()
            .filter(|&[left, _, _, right]| left < right)
            .count();
        Ok(big_dives)
    }
}

impl TryFrom<&str> for Day1 {
    type Error = ParseIntError;
    fn try_from(input: &str) -> Result<Day1, ParseIntError> {
        input
            .split("\n")
            .map(str::parse)
            .collect::<Result<_, _>>()
            .map(|depths| Day1(depths))
    }
}
