// https://adventofcode.com/2021/day/1
use std::{fs::read_to_string, num::ParseIntError};

use crate::types::Answer;
use anyhow::Result;

pub fn solve(input: String) -> Result<(Answer, Answer)> {
    let input = read_input(&input)?;
    Ok((part1(&input), part2(&input)))
}

fn part1(input: &[i64]) -> Result<i64> {
    let mut depth_counter = 0;
    for i in 1..input.len() {
        if input[i - 1] < input[i] {
            depth_counter += 1;
        }
    }

    Ok(depth_counter)
}

fn part2(input: &[i64]) -> Result<i64> {
    let mut depth_counter = 0;
    for i in 1..input.len() - 2 {
        let prev = input[i - 1] + input[i] + input[i + 1];
        let curr = input[i] + input[i + 1] + input[i + 2];
        if prev < curr {
            depth_counter += 1;
        }
    }

    Ok(depth_counter)
}

fn read_input(input: &str) -> Result<Vec<i64>> {
    Ok(input
        .split("\n")
        .map(str::parse::<i64>)
        .collect::<Result<Vec<i64>, ParseIntError>>()?)
}
