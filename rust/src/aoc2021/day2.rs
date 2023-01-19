// https://adventofcode.com/2021/day/2
use anyhow::Result;
use std::num::ParseIntError;

use crate::types::Solver;

pub struct Day2 {
    commands: Vec<SubCmd>,
}

impl Solver for Day2 {
    type Soln1 = i64;
    fn solve_part1(&self) -> Result<i64> {
        Ok(pilot_sub(&self.commands, part1_control_system))
    }

    type Soln2 = i64;
    fn solve_part2(&self) -> Result<Self::Soln2> {
        Ok(pilot_sub(&self.commands, part2_control_system))
    }
}

impl TryFrom<&str> for Day2 {
    type Error = ParseIntError;
    fn try_from(input: &str) -> Result<Self, ParseIntError> {
        input
            .split("\n")
            .map(SubCmd::try_from)
            .collect::<Result<_, _>>()
            .map(|subcmds| Day2 { commands: subcmds })
    }
}

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

impl From<&str> for Direction {
    fn from(value: &str) -> Self {
        match value {
            "forward" => Direction::Forward,
            "up" => Direction::Up,
            "down" => Direction::Down,
            _ => panic!(),
        }
    }
}

struct SubCmd {
    direction: Direction,
    distance: i64,
}

impl TryFrom<&str> for SubCmd {
    type Error = ParseIntError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let words = value.split(" ").collect::<Vec<&str>>();
        let direction = Direction::from(words[0]);
        let distance = words[1].parse::<i64>()?;

        Ok(SubCmd {
            direction,
            distance,
        })
    }
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

fn pilot_sub(commands: &Vec<SubCmd>, maneuver: fn(SubPos, &SubCmd) -> SubPos) -> i64 {
    let mut position = SubPos {
        horizontal: 0,
        depth: 0,
        aim: 0,
    };

    for cmd in commands {
        let new_position = maneuver(position, cmd);
        position = new_position;
    }

    position.horizontal * position.depth
}
