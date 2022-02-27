use std::fs::read_to_string;

use crate::types::{Error, Solution};
use num::abs;

fn read_input(path: &str) -> Result<Vec<i64>, Error> {
    read_to_string(path)?
        .split(',')
        .map(|line| line.trim().parse::<i64>())
        .collect::<Result<_, _>>()
        .map_err(Into::into)
}

pub fn solve(path: &str) -> Result<(Solution, Solution), Error> {
    let crab_positions: Vec<i64> = read_input(path)?;
    let soln1 = part1(&crab_positions);
    let soln2 = part2(&crab_positions);
    Ok((soln1, soln2))
}

fn part1(crab_positions: &Vec<i64>) -> Result<i64, Error> {
    let median = crab_positions
        .get(crab_positions.len() / 2)
        .ok_or(Error::NoSolution)?;
    let cost = crab_positions.iter().map(|x| abs(x - *median)).sum();
    Ok(cost)
}

fn part2(crab_positions: &Vec<i64>) -> Result<i64, Error> {
    let max_pos = *crab_positions.iter().max().ok_or(Error::NoSolution)?;
    let min_pos = *crab_positions.iter().min().ok_or(Error::NoSolution)?;
    let mut min_cost = i64::MAX;
    for pos in min_pos..max_pos + 1 {
        let mut cost = 0;
        for crab_pos in crab_positions {
            cost += part2_fuel_cost(abs(pos - crab_pos))
        }
        if cost < min_cost {
            min_cost = cost;
        }
    }
    Ok(min_cost)
}

fn part2_fuel_cost(dist: i64) -> i64 {
    dist.pow(2) - dist * (dist - 1) / 2
}
