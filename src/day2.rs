use std::{fs::read_to_string, io, num::ParseIntError};

struct SubPos {
    horizontal: i32,
    depth: i32,
    aim: i32,
}

enum SubCmd {
    Forward(i32),
    Up(i32),
    Down(i32),
}

#[derive(Debug)]
pub enum Error {
    IOError(io::Error),
    ParseError(ParseError),
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::IOError(e)
    }
}
impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Error::ParseError(e)
    }
}

#[derive(Debug)]
pub enum ParseError {
    NoValue(ParseIntError),
    Unrecognized(String),
    Malformed(String),
}

impl From<ParseIntError> for ParseError {
    fn from(e: ParseIntError) -> Self {
        ParseError::NoValue(e)
    }
}

pub fn solution(path: &str) -> Result<i32, Error> {
    let sub_cmds: Vec<SubCmd> = read_to_string(path)?
        .split("\n")
        .filter(|&cmd| cmd != "")
        .map(parse_sub_cmd)
        .collect::<Result<_, ParseError>>()?;

    let final_pos = sub_cmds.into_iter().fold(
        SubPos {
            horizontal: 0,
            depth: 0,
            aim: 0,
        },
        apply_sub_cmd,
    );

    Ok(final_pos.horizontal * final_pos.depth)
}

fn apply_sub_cmd(acc: SubPos, cmd: SubCmd) -> SubPos {
    match cmd {
        SubCmd::Forward(x) => SubPos {
            horizontal: acc.horizontal + x,
            depth: x * acc.aim + acc.depth,
            ..acc
        },
        SubCmd::Up(x) => SubPos {
            aim: acc.aim - x,
            ..acc
        },
        SubCmd::Down(x) => SubPos {
            aim: acc.aim + x,
            ..acc
        },
    }
}

fn parse_sub_cmd(cmd_str: &str) -> Result<SubCmd, ParseError> {
    let cmd_strs = cmd_str.split(" ").collect::<Vec<&str>>();
    let instruction = cmd_strs
        .get(0)
        .ok_or(ParseError::Malformed(cmd_str.to_string()))?;
    let value = cmd_strs
        .get(1)
        .ok_or(ParseError::Malformed(cmd_str.to_string()))?
        .parse::<i32>()?;

    match *instruction {
        "forward" => Ok(SubCmd::Forward(value)),
        "up" => Ok(SubCmd::Up(value)),
        "down" => Ok(SubCmd::Down(value)),
        _ => Err(ParseError::Unrecognized(instruction.to_string())),
    }
}
