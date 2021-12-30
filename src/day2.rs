// https://adventofcode.com/2021/day/2
use std::fs::read_to_string;

use crate::types::{Error, Solution};

struct SubPos {
    horizontal: i32,
    depth: i32,
    aim: i32,
}

pub fn solver(path: &str) -> Result<(Solution, Solution), Error> {
    let input = read_to_string(path)?;
    Ok((Ok(5), Ok(5)))
}
fn part2(path: &str) -> Result<i32, Error> {
    let final_pos: SubPos = read_to_string(path)?
        .split("\n")
        .filter(|&cmd| cmd != "")
        .try_fold(
            SubPos {
                horizontal: 0,
                depth: 0,
                aim: 0,
            },
            apply_sub_cmd,
        )?;

    Ok(final_pos.horizontal * final_pos.depth)
}

fn apply_sub_cmd(acc: SubPos, cmd_str: &str) -> Result<SubPos, Error> {
    let cmd_strs = cmd_str.split(" ").collect::<Vec<&str>>();
    if cmd_strs.len() < 2 {
        return Err(Error::Malformed(cmd_str.to_string()));
    }
    let instruction = cmd_strs[0];
    let distance = cmd_strs[1].parse::<i32>()?;

    match instruction {
        "forward" => Ok(SubPos {
            horizontal: acc.horizontal + distance,
            depth: acc.depth + acc.aim * distance,
            ..acc
        }),
        "up" => Ok(SubPos {
            aim: acc.aim - distance,
            ..acc
        }),
        "down" => Ok(SubPos {
            aim: acc.aim + distance,
            ..acc
        }),
        _ => Err(Error::Unrecognized(instruction.to_string())),
    }
}
