use std::{fs::read_to_string, io::Error, num::ParseIntError};

fn main() {
    // day1("input_day1.txt").unwrap();
    let res = day2("input_day2.txt").unwrap();
    println!("{}", res)
}

fn day1(path: &str) -> Result<i32, Error> {
    let input = read_to_string(path)?
        .split("\n")
        .map(str::parse::<i32>)
        .flatten()
        .collect::<Vec<i32>>();

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

#[derive(Debug)]
enum Day2Error {
    IOError(Error),
    SubCmdError(SubCmdError),
}

impl From<Error> for Day2Error {
    fn from(e: Error) -> Self {
        Day2Error::IOError(e)
    }
}
impl From<SubCmdError> for Day2Error {
    fn from(e: SubCmdError) -> Self {
        Day2Error::SubCmdError(e)
    }
}

enum SubCmd {
    Forward(i32),
    Up(i32),
    Down(i32),
}

fn day2(path: &str) -> Result<i32, Day2Error> {
    let sub_cmds = read_to_string(path)?
        .split("\n")
        .filter(|&cmd| cmd != "")
        .map(parse_submarine_command)
        .collect::<Result<Vec<SubCmd>, SubCmdError>>()?;
    let final_pos = sub_cmds.into_iter().fold(
        SubPos {
            horizontal: 0,
            depth: 0,
            aim: 0,
        },
        apply_submarine_command,
    );

    Ok(final_pos.horizontal * final_pos.depth)
}

struct SubPos {
    horizontal: i32,
    depth: i32,
    aim: i32,
}

fn apply_submarine_command(acc: SubPos, cmd: SubCmd) -> SubPos {
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

#[derive(Debug)]
enum SubCmdError {
    ParseIntError(ParseIntError),
    UnrecognizedInstruction(String),
    MalformedCommand(String),
}

impl From<ParseIntError> for SubCmdError {
    fn from(e: ParseIntError) -> Self {
        SubCmdError::ParseIntError(e)
    }
}

fn parse_submarine_command(cmd_str: &str) -> Result<SubCmd, SubCmdError> {
    let cmd_strs = cmd_str.split(" ").collect::<Vec<&str>>();
    let instruction = cmd_strs
        .get(0)
        .ok_or(SubCmdError::MalformedCommand(cmd_str.to_string()))?;
    let value = cmd_strs
        .get(1)
        .ok_or(SubCmdError::MalformedCommand(cmd_str.to_string()))?
        .parse::<i32>()?;

    match *instruction {
        "forward" => Ok(SubCmd::Forward(value)),
        "up" => Ok(SubCmd::Up(value)),
        "down" => Ok(SubCmd::Down(value)),
        _ => Err(SubCmdError::UnrecognizedInstruction(
            instruction.to_string(),
        )),
    }
}
