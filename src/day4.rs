use std::{fs::read_to_string, io::BufRead};

use crate::{day3::get_column, types::Error};

#[derive(Clone, Copy)]
struct BingoSpace {
    value: i32,
    marked: bool,
}

impl BingoSpace {
    fn new(value: i32) -> Self {
        BingoSpace {
            value,
            marked: false,
        }
    }
}

type Board = Vec<Vec<BingoSpace>>;

pub fn solver(path: &str) -> Result<(Result<i32, Error>, Result<i32, Error>), Error> {
    let (draws, mut boards) = read_input(path)?;
    let mut soln1 = None;
    let mut soln2 = None;
    for draw in draws {
        for b in &mut boards {
            mark_board(b, draw);
        }
        if soln1.is_none() {
            soln1 = part1(draw, &boards);
        }
        if soln2.is_none() {
            soln2 = part2(draw, &boards);
        }
        boards = boards.into_iter().filter(|b| !won(b)).collect();
        if boards.len() == 0 {
            break;
        }
    }
    Ok((soln1.ok_or(Error::Example), soln2.ok_or(Error::Example)))
}

fn read_input(path: &str) -> Result<(Vec<i32>, Vec<Board>), Error> {
    let input = read_to_string(path)?;
    let lines: Vec<&str> = input.split("\n").collect();
    let draws = lines[0]
        .split(",")
        .map(|s| s.parse())
        .collect::<Result<Vec<i32>, _>>()?;
    let boards = lines[2..lines.len() - 1]
        .split(|s| *s == "")
        .map(read_input_board)
        .collect::<Result<Vec<Board>, _>>()?;
    Ok((draws, boards))
}

fn read_input_board(text: &[&str]) -> Result<Board, Error> {
    text.iter()
        .map(|line| read_input_board_line(*line))
        .collect::<Result<Board, _>>()
}

fn read_input_board_line(line: &str) -> Result<Vec<BingoSpace>, Error> {
    line.split_whitespace()
        .map(|str_value| {
            let value = str_value.parse()?;
            Ok(BingoSpace::new(value))
        })
        .collect()
}

fn part1(curr_draw: i32, boards: &[Board]) -> Option<i32> {
    boards
        .iter()
        .find(|&b| won(b))
        .and_then(|winning_board| Some(calc_score(winning_board, curr_draw)))
}

fn part2(curr_draw: i32, boards: &[Board]) -> Option<i32> {
    let losing_board = &boards.first()?;
    if boards.len() == 1 && won(losing_board) {
        let score = calc_score(losing_board, curr_draw);
        return Some(score);
    }

    None
}

fn mark_board(board: &mut Board, x: i32) {
    for row in board {
        for space in row {
            if space.value == x {
                space.marked = true;
                return;
            }
        }
    }
}

fn won(board: &Board) -> bool {
    if let Some(first_row) = board.first() {
        let completed_row = board.iter().find(|&x| completed(x));
        let completed_col = first_row
            .iter()
            .enumerate()
            .map(|(i, _)| get_column(board.iter(), i))
            .find(|col| completed(&col));
        completed_row.is_some() || completed_col.is_some()
    } else {
        false
    }
}

fn completed(spaces: &[BingoSpace]) -> bool {
    spaces.iter().fold(true, |acc, space| acc && space.marked)
}

fn calc_score(board: &Board, last_draw: i32) -> i32 {
    let sum = board.iter().fold(0, |acc, curr| {
        acc + curr.iter().fold(
            0,
            |acc, curr| if !curr.marked { acc + curr.value } else { acc },
        )
    });

    sum * last_draw
}
