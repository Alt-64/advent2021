use std::{fs::read_to_string, num::ParseIntError};

use crate::types::{Error, Solution};
use num::abs;

pub fn solver(path: &str) -> Result<(Solution, Solution), Error> {
    let mut crabs: Vec<i64> = read_to_string(path)?
        .split(',')
        .map(str::trim)
        .map(str::parse)
        .collect::<Result<_, ParseIntError>>()?;
    let soln1 = part1(&mut crabs);
    let soln2 = part2(&crabs).or(Some(0)).unwrap();
    Ok((Ok(soln1), Ok(soln2)))
}

fn part1(crabs: &Vec<i64>) -> i64 {
    let median = crabs[crabs.len() / 2];
    crabs.iter().map(|x| abs(x - median)).sum()
}

fn part2(crabs: &Vec<i64>) -> Option<i64> {
    let max_pos = *crabs.iter().max()?;
    let min_pos = *crabs.iter().min()?;
    (min_pos..max_pos + 1)
        .map(|x| crabs.iter().map(|c| part2_fuel_cost(abs(x - c))).sum())
        .min()
}

fn part2_fuel_cost(dist: i64) -> i64 {
    dist.pow(2) - dist * (dist - 1) / 2
}
