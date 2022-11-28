use std::num::ParseIntError;

use anyhow::{anyhow, Result};

use crate::types::{Answer, NoSolutionError};
use num::abs;

pub fn solve(input: &str) -> Result<(Answer, Answer)> {
    let crabs: Vec<i64> = read_input(input)?;
    let soln1 = part1(&crabs).ok_or(anyhow!(NoSolutionError));
    let soln2 = part2(&crabs).ok_or(anyhow!(NoSolutionError));
    Ok((Box::new(soln1), Box::new(soln2)))
}

fn part1(crabs: &Vec<i64>) -> Option<i64> {
    let &median = crabs.get(crabs.len() / 2)?;
    Some(crabs.iter().map(|&crab| get_distance(crab, median)).sum())
}

fn get_distance(x: i64, y: i64) -> i64 {
    abs(x - y)
}

fn part2(crabs: &Vec<i64>) -> Option<i64> {
    let min = crabs.iter().min()?.to_owned();
    let max = crabs.iter().max()?.to_owned();

    return (min..=max)
        .map(|pos| crabs.iter().map(|&crab| cost_to_move(crab, pos)).sum())
        .min();
}

fn cost_to_move(crab: i64, dest: i64) -> i64 {
    fuel_cost(get_distance(crab, dest))
}

fn fuel_cost(dist: i64) -> i64 {
    dist.pow(2) - dist * (dist - 1) / 2
}

fn read_input(input: &str) -> Result<Vec<i64>, ParseIntError> {
    input
        .split(',')
        .map(|line| line.trim().parse::<i64>())
        .collect::<Result<_, ParseIntError>>()
}
