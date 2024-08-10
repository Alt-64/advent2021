// https://adventofcode.com/2021/day/2
use std::num::ParseIntError;
use std::str::FromStr;

#[derive(Default)]
struct Dive {
    commands: Vec<SubCmd>,
    part: usize,
}

impl FromStr for Dive {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let commands: Vec<_> = s
            .split("\n")
            .map(SubCmd::try_from)
            .collect::<Result<_, _>>()?;
        return Ok(Dive {
            commands,
            ..Self::default()
        });
    }
}

impl Iterator for Dive {
    type Item = (usize, i64);

    fn next(&mut self) -> Option<Self::Item> {
        self.part += 1;
        match self.part {
            1 => Some((self.part, part_1(self))),
            2 => Some((self.part, part_2(self))),
            _ => None,
        }
    }
}

fn part_1(dive: &Dive) -> i64 {
    let final_pos = dive
        .commands
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
            Direction::None => pos,
        });
    final_pos.horizontal * final_pos.depth
}

fn part_2(dive: &Dive) -> i64 {
    let final_pos = dive
        .commands
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
            Direction::None => pos,
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
    None,
}

impl From<&str> for Direction {
    fn from(value: &str) -> Self {
        match value {
            "forward" => Direction::Forward,
            "up" => Direction::Up,
            "down" => Direction::Down,
            _ => Direction::None,
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

#[cfg(test)]
mod tests {
    use super::*;

    const DAY: usize = 2;

    #[test]
    fn solve() {
        let input = include_str!("day02.txt");
        let dive = Dive::from_str(input).expect("failed to read input string");
        for (i, result) in dive {
            println!("Day {DAY} Part {i} {result}")
        }
    }
}
