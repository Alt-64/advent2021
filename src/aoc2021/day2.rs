// https://adventofcode.com/2021/day/2

use crate::types::{Answer, BadInputError};
use anyhow::Result;

struct SubPos {
    horizontal: i64,
    depth: i64,
    aim: i64,
}

enum Direction {
    Forward,
    Up,
    Down,
}

struct SubCmd {
    direction: Direction,
    distance: i64,
}

pub fn solve(input: String) -> Result<(Answer, Answer)> {
    let commands: Vec<&str> = input.split("\n").filter(|&cmd| cmd != "").collect();

    Ok((
        maneuver_sub(&commands, part1_control),
        maneuver_sub(&commands, part2_control),
    ))
}

fn part1_control(acc: SubPos, cmd: SubCmd) -> SubPos {
    match cmd.direction {
        Direction::Forward => SubPos {
            horizontal: acc.horizontal + cmd.distance,
            ..acc
        },
        Direction::Up => SubPos {
            depth: acc.depth - cmd.distance,
            ..acc
        },
        Direction::Down => SubPos {
            depth: acc.depth + cmd.distance,
            ..acc
        },
    }
}

fn part2_control(acc: SubPos, cmd: SubCmd) -> SubPos {
    match cmd.direction {
        Direction::Forward => SubPos {
            horizontal: acc.horizontal + cmd.distance,
            depth: acc.depth + acc.aim * cmd.distance,
            ..acc
        },
        Direction::Up => SubPos {
            aim: acc.aim - cmd.distance,
            ..acc
        },
        Direction::Down => SubPos {
            aim: acc.aim + cmd.distance,
            ..acc
        },
    }
}

fn maneuver_sub(commands: &[&str], control: fn(SubPos, SubCmd) -> SubPos) -> Result<i64> {
    let mut pos = SubPos {
        horizontal: 0,
        depth: 0,
        aim: 0,
    };

    for cmd in commands {
        pos = control(pos, parse_command(cmd)?)
    }

    Ok(pos.horizontal * pos.depth)
}

fn parse_command<'a>(cmd: &&'a str) -> Result<SubCmd> {
    let words = cmd.split(" ").collect::<Vec<&str>>();
    if words.len() < 2 {
        return Err(BadInputError(words.join("")))?;
    }
    let direction = parse_direction(words[0])?;
    let distance = words[1].parse::<i64>()?;

    Ok(SubCmd {
        direction,
        distance,
    })
}

fn parse_direction(dir: &str) -> Result<Direction> {
    Ok(match dir {
        "forward" => Direction::Forward,
        "up" => Direction::Up,
        "down" => Direction::Down,
        _ => return Err(BadInputError(dir.to_string()))?,
    })
}
