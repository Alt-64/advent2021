use std::fs::read_to_string;

use crate::{
    day3::get_column,
    types::{Error, Solution},
};

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

type Board = [[BingoSpace; 5]; 5];

pub fn solver(path: &str) -> Result<(Solution, Solution), Error> {
    let (draws, mut boards) = read_input(path)?;
    let mut soln1 = None;
    let mut soln2 = None;
    for curr_draw in draws {
        for b in &mut boards {
            mark_board(b, curr_draw);
        }

        soln1 = soln1.or(part1(curr_draw, &boards));
        soln2 = soln2.or(part2(curr_draw, &boards));

        boards = boards.into_iter().filter(|b| !won(b)).collect();
        if boards.len() == 0 {
            break;
        }
    }
    Ok((
        soln1.ok_or(Error::NoSolution),
        soln2.ok_or(Error::NoSolution),
    ))
}

fn part1(curr_draw: i32, boards: &[Board]) -> Option<i32> {
    boards
        .iter()
        .find(|&b| won(b))
        .and_then(|winning_board| Some(calc_score(winning_board, curr_draw)))
}

fn part2(curr_draw: i32, boards: &[Board]) -> Option<i32> {
    let remaining_board = &boards.first()?;
    let score = calc_score(remaining_board, curr_draw);
    return Some(score);
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
        .collect::<Result<Vec<[BingoSpace; 5]>, _>>()?
        .as_slice()
        .try_into()
        .map_err(Error::from)
}

fn read_input_board_line(line: &str) -> Result<[BingoSpace; 5], Error> {
    line.split_whitespace()
        .map(|str_value| {
            let value = str_value.parse()?;
            Ok(BingoSpace::new(value))
        })
        .collect::<Result<Vec<BingoSpace>, Error>>()?
        .as_slice()
        .try_into()
        .map_err(Error::from)
}
