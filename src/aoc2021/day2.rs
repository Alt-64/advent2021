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

pub fn solve(input: &str) -> Result<(Answer, Answer)> {
    let commands = read_input(input)?;

    Ok((
        pilot_sub(&commands, part1_control_system),
        pilot_sub(&commands, part2_control_system),
    ))
}

fn part1_control_system(acc: SubPos, cmd: &SubCmd) -> SubPos {
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

fn part2_control_system(acc: SubPos, cmd: &SubCmd) -> SubPos {
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

fn pilot_sub(commands: &Vec<SubCmd>, maneuver: fn(SubPos, &SubCmd) -> SubPos) -> Result<i64> {
    let mut position = SubPos {
        horizontal: 0,
        depth: 0,
        aim: 0,
    };

    for cmd in commands {
        let new_position = maneuver(position, cmd);
        position = new_position;
    }

    Ok(position.horizontal * position.depth)
}

fn read_input(input: &str) -> Result<Vec<SubCmd>> {
    input.split("\n").map(parse_command).collect()
}

fn parse_command(cmd: &str) -> Result<SubCmd> {
    let words = cmd.split(" ").collect::<Vec<&str>>();
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
        _ => return Err(BadInputError(dir.to_string()).into()),
    })
}
