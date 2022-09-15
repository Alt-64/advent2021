// https://adventofcode.com/2021/day/4
use std::{convert::TryFrom, num::ParseIntError};

use {super::day3::get_column, crate::types::Answer};

use anyhow::Result;

use crate::types::{BadInputError, NoSolutionError};

#[derive(Clone, Copy, Debug)]
struct BingoSpace {
    value: i64,
    marked: bool,
}

impl BingoSpace {
    fn new(value: i64) -> Self {
        BingoSpace {
            value,
            marked: false,
        }
    }
}

type BingoBoard = Vec<Vec<BingoSpace>>;

struct Bingo {
    boards: Vec<BingoBoard>,
    draws: Vec<i64>,
}

pub fn solve(input: &str) -> Result<(Answer, Answer)> {
    let Bingo { mut boards, draws } = Bingo::try_from(input)?;
    let mut soln1 = None;
    let mut soln2 = None;
    for curr_draw in draws {
        for b in &mut boards {
            mark_board(b, curr_draw);
        }

        if soln1.is_none() {
            soln1 = part1(curr_draw, &boards);
        }
        if soln2.is_none() {
            soln2 = part2(curr_draw, &boards);
        }

        boards = boards
            .into_iter()
            .filter(|b| !find_completed_set(b).is_some())
            .collect();
    }
    Ok((
        soln1.ok_or(NoSolutionError.into()),
        soln2.ok_or(NoSolutionError.into()),
    ))
}

fn part1(draw: i64, boards: &[BingoBoard]) -> Option<i64> {
    for board in boards {
        if find_completed_set(board).is_some() {
            return Some(calc_score(board, draw));
        }
    }
    return None;
}

fn part2(curr_draw: i64, boards: &[BingoBoard]) -> Option<i64> {
    let remaining_board = &boards.first()?;
    let score = calc_score(remaining_board, curr_draw);
    return Some(score);
}

fn mark_board(board: &mut BingoBoard, x: i64) {
    for row in board {
        for space in row {
            if space.value == x {
                space.marked = true;
                return;
            }
        }
    }
}

fn find_completed_set(board: &BingoBoard) -> Option<Vec<BingoSpace>> {
    if let Some(first_row) = board.first() {
        let completed_row = board.iter().find(|&row| completed(row)).cloned();
        let completed_col = first_row
            .iter()
            .enumerate()
            .map(|(i, _)| get_column(board, i))
            .find(|col| completed(col));
        completed_row.or(completed_col)
    } else {
        None
    }
}

fn completed(spaces: &[BingoSpace]) -> bool {
    spaces.iter().fold(true, |acc, space| acc && space.marked)
}

fn calc_score(board: &BingoBoard, last_draw: i64) -> i64 {
    let sum = board.iter().fold(0, |acc, curr| {
        acc + curr.iter().fold(
            0,
            |acc, curr| if !curr.marked { acc + curr.value } else { acc },
        )
    });

    sum * last_draw
}

impl TryFrom<&str> for Bingo {
    fn try_from(input: &str) -> std::result::Result<Self, Self::Error> {
        let mut inputs = input.split("\n\n");
        let first_line = inputs.next().ok_or(BadInputError("No line".to_string()))?;

        let draws = read_input_draws(first_line)?;
        let boards = inputs
            .map(read_input_board)
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Bingo { draws, boards })
    }

    type Error = anyhow::Error;
}

impl TryFrom<&str> for BingoSpace {
    type Error = ParseIntError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse().map(BingoSpace::new)
    }
}

fn read_input_draws(line: &str) -> Result<Vec<i64>, ParseIntError> {
    line.split(',').map(str::parse).collect()
}

fn read_input_board(board_str: &str) -> Result<BingoBoard, ParseIntError> {
    board_str.split('\n').map(read_input_board_line).collect()
}

fn read_input_board_line(line: &str) -> Result<Vec<BingoSpace>, ParseIntError> {
    line.split_whitespace().map(BingoSpace::try_from).collect()
}
