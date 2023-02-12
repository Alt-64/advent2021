use std::num::ParseIntError;

use std::fmt::Debug;

use crate::types::{SolveState, Solver};
pub struct Day7 {
    state: SolveState,
    crabs: Vec<u16>,
}

impl Solver<'_> for Day7 {
    type Soln1 = Option<u16>;
    fn solve_part1(&mut self) -> Self::Soln1 {
        let &median_crab = self.crabs.get(self.crabs.len() / 2)?;
        let distances_to_median_crab = self
            .crabs
            .iter()
            .map(|&crab| get_distance(crab, median_crab))
            .sum();
        Some(distances_to_median_crab)
    }

    type Soln2 = Option<u16>;
    fn solve_part2(&mut self) -> Self::Soln2 {
        let min = self.crabs.iter().min().cloned()?;
        let max = self.crabs.iter().max().cloned()?;

        let minimum_distance_to_align_crabs = (min..=max)
            .map(|pos| self.crabs.iter().map(|&crab| cost_to_move(crab, pos)).sum())
            .min();
        return minimum_distance_to_align_crabs;
    }
}

fn get_distance(x: u16, y: u16) -> u16 {
    u16::abs_diff(x, y)
}

fn cost_to_move(crab: u16, dest: u16) -> u16 {
    fuel_cost(get_distance(crab, dest))
}

fn fuel_cost(dist: u16) -> u16 {
    dist.pow(2) - dist * (dist - 1) / 2
}

impl TryFrom<&str> for Day7 {
    type Error = ParseIntError;

    fn try_from(value: &str) -> std::result::Result<Self, Self::Error> {
        let crabs = value
            .split(',')
            .map(|line| line.trim().parse::<u16>())
            .collect::<Result<_, _>>()?;
        Ok(Day7 {
            state: SolveState::new(),
            crabs,
        })
    }
}

impl Iterator for Day7 {
    type Item = Box<dyn Debug>;

    fn next(&mut self) -> Option<Self::Item> {
        self.state.next()
    }
}
