// https://adventofcode.com/2021/day/2
use std::num::ParseIntError;
use std::sync::mpsc::Sender;
use std::thread;

use crate::types::Solution;

fn solve(input: &str, tx: Sender<(usize, usize, Solution)>) -> anyhow::Result<()> {
    let commands: Vec<_> = input
        .split("\n")
        .map(SubCmd::try_from)
        .collect::<Result<_, _>>()?;

    let tx_1 = tx.clone();
    let handle = thread::spawn(move || tx_1.send((1, 1, Ok(Box::new(part_1(commands))))));
    tx.send((1, 1, Ok(Box::new(part_2(commands)))))?;

    handle.join().unwrap().map_err(Into::into)
}

fn part_1(commands: Vec<SubCmd>) -> i64 {
    let final_pos = commands
        .iter()
        .fold(SubPos::default(), |pos, cmd| match cmd.direction {
            Direction::Forward => SubPos {
                horizontal: pos.horizontal + cmd.distance,
                ..pos
            },
            Direction::Up => SubPos {
                depth: pos.depth - cmd.distance,
                ..pos
            },
            Direction::Down => SubPos {
                depth: pos.depth + cmd.distance,
                ..pos
            },
        });
    final_pos.horizontal * final_pos.depth
}

fn part_2(commands: Vec<SubCmd>) -> i64 {
    let final_pos = commands
        .iter()
        .fold(SubPos::default(), |pos, cmd| match cmd.direction {
            Direction::Forward => SubPos {
                horizontal: pos.horizontal + cmd.distance,
                depth: pos.depth + pos.aim * cmd.distance,
                ..pos
            },
            Direction::Up => SubPos {
                aim: pos.aim - cmd.distance,
                ..pos
            },
            Direction::Down => SubPos {
                aim: pos.aim + cmd.distance,
                ..pos
            },
        });
    final_pos.horizontal * final_pos.depth
}
struct SubPos {
    horizontal: i64,
    depth: i64,
    aim: i64,
}

impl Default for SubPos {
    fn default() -> Self {
        Self {
            horizontal: 0,
            depth: 0,
            aim: 0,
        }
    }
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
