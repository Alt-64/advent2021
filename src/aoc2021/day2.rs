// https://adventofcode.com/2021/day/2
use std::fs::read_to_string;

use crate::types::{Error, Solution};

struct SubPos {
    horizontal: i64,
    depth: i64,
    aim: i64,
}

struct SubCmd<'a> {
    direction: &'a str,
    distance: i64,
}

pub fn solve(path: &str) -> Result<(Solution, Solution), Error> {
    let input = read_to_string(path)?;
    let commands: Vec<&str> = input.split("\n").filter(|&cmd| cmd != "").collect();
    Ok((
        maneuver_sub(&commands, part1_controls),
        maneuver_sub(&commands, part2_controls),
    ))
}

fn part1_controls(acc: SubPos, cmd: SubCmd) -> Result<SubPos, Error> {
    match cmd.direction {
        "forward" => Ok(SubPos {
            horizontal: acc.horizontal + cmd.distance,
            ..acc
        }),
        "up" => Ok(SubPos {
            depth: acc.depth - cmd.distance,
            ..acc
        }),
        "down" => Ok(SubPos {
            depth: acc.depth + cmd.distance,
            ..acc
        }),
        _ => Err(Error::Unrecognized(cmd.direction.to_string())),
    }
}

fn part2_controls(acc: SubPos, cmd: SubCmd) -> Result<SubPos, Error> {
    match cmd.direction {
        "forward" => Ok(SubPos {
            horizontal: acc.horizontal + cmd.distance,
            depth: acc.depth + acc.aim * cmd.distance,
            ..acc
        }),
        "up" => Ok(SubPos {
            aim: acc.aim - cmd.distance,
            ..acc
        }),
        "down" => Ok(SubPos {
            aim: acc.aim + cmd.distance,
            ..acc
        }),
        _ => Err(Error::Unrecognized(cmd.direction.to_string())),
    }
}

fn maneuver_sub(
    commands: &[&str],
    controls: fn(SubPos, SubCmd) -> Result<SubPos, Error>,
) -> Result<i64, Error> {
    let final_pos: SubPos = commands
        .iter()
        .map(parse_command)
        .collect::<Result<Vec<SubCmd>, Error>>()?
        .into_iter()
        .try_fold(
            SubPos {
                horizontal: 0,
                depth: 0,
                aim: 0,
            },
            controls,
        )?;

    Ok(final_pos.horizontal * final_pos.depth)
}

fn parse_command<'a>(cmd_str: &&'a str) -> Result<SubCmd<'a>, Error> {
    let cmd_strs = cmd_str.split(" ").collect::<Vec<&str>>();
    if cmd_strs.len() < 2 {
        return Err(Error::BadInput(cmd_str.to_string()));
    }
    let direction = cmd_strs[0];
    let distance = cmd_strs[1].parse::<i64>()?;

    Ok(SubCmd {
        direction,
        distance,
    })
}
