// https://adventofcode.com/2021/day/1

use std::{num::ParseIntError, str::FromStr};

#[derive(Default)]
pub struct SonarSweep {
    depths: Vec<u16>,
    part: usize,
}

impl FromStr for SonarSweep {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let depths = s
            .trim()
            .split("\n")
            .map(str::parse::<u16>)
            .collect::<Result<_, _>>()?;
        return Ok(SonarSweep {
            depths,
            ..Self::default()
        });
    }
}

impl Iterator for SonarSweep {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        self.part += 1;
        match self.part {
            1 => Some((self.part, part_1(self))),
            2 => Some((self.part, part_2(self))),
            _ => None,
        }
    }
}

fn part_1(sweep: &SonarSweep) -> usize {
    sweep
        .depths
        .array_windows::<2>()
        .filter(|&[left, right]| left < right)
        .count()
}

fn part_2(sweep: &SonarSweep) -> usize {
    sweep
        .depths
        .array_windows::<4>()
        .filter(|&[left, _, _, right]| left < right)
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;

    const DAY: usize = 1;

    #[test]
    fn solve() {
        let input = include_str!("day01.txt");
        let sweep = SonarSweep::from_str(input).expect("failed to read input string");
        for (i, result) in sweep {
            println!("Day {DAY} Part {i} {result}")
        }
    }
}
