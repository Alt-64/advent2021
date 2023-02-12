// https://adventofcode.com/2021/day/2
use crate::types::{SolveState, Solver};
use anyhow::Result;
use std::fmt::Debug;
use std::num::ParseIntError;

pub struct Day2 {
    state: SolveState,
    commands: Vec<SubCmd>,
}
impl TryFrom<&str> for Day2 {
    type Error = ParseIntError;
    fn try_from(input: &str) -> Result<Self, ParseIntError> {
        Ok(Day2 {
            state: SolveState::new(),
            commands: input
                .split("\n")
                .map(SubCmd::try_from)
                .collect::<Result<_, _>>()?,
        })
    }
}

impl Solver<'_> for Day2 {
    type Soln1 = i64;
    fn solve_part1(&mut self) -> Self::Soln1 {
        pilot_sub(&self.commands, part1_control_system)
    }

    type Soln2 = i64;
    fn solve_part2(&mut self) -> Self::Soln2 {
        pilot_sub(&self.commands, part2_control_system)
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

impl Iterator for Day2 {
    type Item = Box<dyn Debug>;

    fn next(&mut self) -> Option<Self::Item> {
        self.state.next()
    }
}
