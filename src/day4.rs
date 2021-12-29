use std::fs::read_to_string;

use crate::{day3::get_column, errors::Error};

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

fn read_input(path: &str) -> Result<(Vec<i32>, Vec<Board>), Error> {
    let input_str = read_to_string(path)?;
    let input: Vec<&str> = input_str.split("\n").collect();
    let draws = input[0]
        .split(",")
        .map(|s| s.parse())
        .collect::<Result<Vec<i32>, _>>()?;
    let boards: Vec<Board> = input[2..input.len() - 1]
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

pub fn part1(path: &str) -> Result<i32, Error> {
    let (draws, mut boards) = read_input(path)?;
    for draw in draws {
        for b in &mut boards {
            mark_board(b, draw);
        }
        if let Some(winning_board) = boards.iter().find(|&b| won(b)) {
            let score = calc_score(winning_board, draw);
            return Ok(score);
        }
    }
    Err(Error::Malformed(
        "no winning board configuration".to_string(),
    ))
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

fn completed(spaces: &[BingoSpace]) -> bool {
    spaces.iter().fold(true, |acc, space| acc && space.marked)
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

fn calc_score(board: &Board, last_draw: i32) -> i32 {
    let sum = board.iter().fold(0, |acc, curr| {
        acc + curr.iter().fold(
            0,
            |acc, curr| if !curr.marked { acc + curr.value } else { acc },
        )
    });

    sum * last_draw
}

// ------------------------------------

pub fn part2(path: &str) -> Result<i32, Error> {
    let (draws, mut boards) = read_input(path)?;
    for draw in draws {
        for b in &mut boards {
            mark_board(b, draw);
        }
        if boards.len() == 1 {
            let losing_board = &boards[0];
            if won(losing_board) {
                let score = calc_score(losing_board, draw);
                return Ok(score);
            }
        }
        boards = boards.into_iter().filter(|b| !won(b)).collect();
    }
    Err(Error::Malformed("no loser board configuration".to_string()))
}
