// https://adventofcode.com/2021/day/1
use std::fs::read_to_string;

use crate::errors::Error;

fn read_input(path: &str) -> Result<Vec<i32>, std::io::Error> {
    let input = read_to_string(path)?
        .split("\n")
        .map(str::parse::<i32>)
        .flatten()
        .collect::<Vec<i32>>();
    Ok(input)
}

pub fn part1(path: &str) -> Result<i32, Error> {
    let input = read_input(path)?;

    let mut depth_counter = 0;
    for i in 1..input.len() {
        let prev = input[i - 1];
        let curr = input[i];
        if prev < curr {
            depth_counter += 1;
        }
    }

    Ok(depth_counter)
}

pub fn part2(path: &str) -> Result<i32, Error> {
    let input = read_input(path)?;

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
