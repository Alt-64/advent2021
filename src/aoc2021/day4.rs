// https://adventofcode.com/2021/day/4
use std::fs::read_to_string;

use {
    super::day3::get_column,
    crate::types::{Error, Solution},
};

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

type Board = [[BingoSpace; 5]; 5];

pub fn solve(path: &str) -> Result<(Solution, Solution), Error> {
    let (draws, mut boards) = read_input(path)?;
    let mut soln1 = None;
    let mut soln2 = None;
    for curr_draw in draws {
        for b in &mut boards {
            mark_board(b, curr_draw);
        }

        soln1 = soln1.or(part1(curr_draw, &boards));
        soln2 = soln2.or(part2(curr_draw, &boards));

        boards = boards
            .into_iter()
            .filter(|b| !find_completed_set(b).is_some())
            .collect();
        if boards.len() == 0 {
            break;
        }
    }
    Ok((
        soln1.ok_or(Error::NoSolution),
        soln2.ok_or(Error::NoSolution),
    ))
}

fn part1(curr_draw: i64, boards: &[Board]) -> Option<i64> {
    for b in boards {
        if find_completed_set(b).is_some() {
            return Some(calc_score(b, curr_draw));
        }
    }
    return None;
}

fn part2(curr_draw: i64, boards: &[Board]) -> Option<i64> {
    let remaining_board = &boards.first()?;
    let score = calc_score(remaining_board, curr_draw);
    return Some(score);
}

fn mark_board(board: &mut Board, x: i64) {
    for row in board {
        for space in row {
            if space.value == x {
                space.marked = true;
                return;
            }
        }
    }
}

fn find_completed_set(board: &Board) -> Option<[BingoSpace; 5]> {
    if let Some(first_row) = board.first() {
        let completed_row = board.iter().find(|row| completed(*row)).cloned();
        let completed_col = first_row
            .iter()
            .enumerate()
            .map(|(i, _)| get_column(board.iter(), i))
            .find(|col| completed(&col))
            .map(|col| col.try_into().unwrap());
        completed_row.or(completed_col)
    } else {
        None
    }
}

fn completed(spaces: &[BingoSpace]) -> bool {
    spaces.iter().fold(true, |acc, space| acc && space.marked)
}

fn calc_score(board: &Board, last_draw: i64) -> i64 {
    let sum = board.iter().fold(0, |acc, curr| {
        acc + curr.iter().fold(
            0,
            |acc, curr| if !curr.marked { acc + curr.value } else { acc },
        )
    });

    sum * last_draw
}

fn read_input(path: &str) -> Result<(Vec<i64>, Vec<Board>), Error> {
    let input = read_to_string(path)?;
    let lines: Vec<&str> = input.split('\n').collect();
    let draws: Vec<i64> = lines[0]
        .split(',')
        .map(str::parse)
        .collect::<Result<_, _>>()?;
    let boards: Vec<Board> = lines[2..lines.len() - 1]
        .split(|s| *s == "")
        .map(read_input_board)
        .collect::<Result<_, _>>()?;
    Ok((draws, boards))
}

fn read_input_board(text: &[&str]) -> Result<Board, Error> {
    text.iter()
        .map(|line| read_input_board_line(*line))
        .collect::<Result<Vec<[BingoSpace; 5]>, _>>()?
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
        .try_into()
        .map_err(Error::from)
}
