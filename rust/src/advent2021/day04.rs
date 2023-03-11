// https://adventofcode.com/2021/day/4
use crate::types::{expect_soln, BadInputError, NoSolutionError, Solution};
use std::sync::mpsc::Sender;
use std::{convert::TryFrom, num::ParseIntError};

fn solve(input: &str, tx: Sender<(usize, usize, Solution)>) -> anyhow::Result<()> {
    let inputs = input.split("\n\n");

    let first_line = inputs.next().ok_or(BadInputError("No line".to_string()))?;
    let draws = read_input_draws(first_line)?;

    let players: Vec<_> = inputs.map(Player::try_from).collect::<Result<_, _>>()?;

    let bingo = play_bingo(draws, &mut players);

    let first_winner = bingo.next();
    tx.send((4, 1, expect_soln(first_winner)))?;

    let last_winner = bingo.last();
    tx.send((4, 2, expect_soln(last_winner)))?;

    Ok(())
}

fn read_input_draws(line: &str) -> Result<Vec<usize>, ParseIntError> {
    line.split(',').map(str::parse).collect()
}

fn read_input_board_line(line: &str) -> Result<Vec<(usize, bool)>, ParseIntError> {
    line.split_whitespace()
        .map(|value| value.parse::<usize>().map(|value| (value, false)))
        .collect()
}

fn read_board(input: &str) -> Result<Vec<Vec<(usize, bool)>>, ParseIntError> {
    input
        .split('\n')
        .map(read_input_board_line)
        .collect::<Result<_, _>>()
}

enum Player {
    Playing(Vec<Vec<(usize, bool)>>),
    Won(usize),
}

impl Player {
    fn play_round(&mut self, draw: usize) {
        *self = match self {
            Player::Playing(board) => {
                mark_cell(board, draw);
                match get_score(board, draw) {
                    Some(score) => Player::Won(score),
                    None => *self,
                }
            }
            Player::Won(_) => *self,
        }
    }

    fn has_won(&self) -> Option<usize> {
        match self {
            Player::Playing(_) => None,
            Player::Won(score) => Some(*score),
        }
    }
}

impl TryFrom<&str> for Player {
    type Error = ParseIntError;

    fn try_from(input: &str) -> std::result::Result<Self, Self::Error> {
        read_board(input).map(Player::Playing)
    }
}

fn play_bingo<'a>(
    draws: Vec<usize>,
    players: &'a mut Vec<Player>,
) -> Box<dyn Iterator<Item = usize>> {
    Box::new(draws.into_iter().flat_map(move |draw| {
        players.iter_mut().filter_map(|player| {
            player.play_round(draw);
            player.has_won()
        })
    }))
}

fn mark_cell(board: &mut Vec<Vec<(usize, bool)>>, draw: usize) {
    for row in board {
        for &mut (value, marked) in row {
            if value == draw {
                marked = true;
            }
        }
    }
}

fn get_score(board: &Vec<Vec<(usize, bool)>>, draw: usize) -> Option<usize> {
    find_completed_set(board).map(|_| draw * count_marked_spaces(board))
}

fn find_completed_set(board: &Vec<Vec<(usize, bool)>>) -> Option<Vec<&(usize, bool)>> {
    find_completed_row(board).or_else(|| find_completed_col(board))
}

fn find_completed_row(board: &Vec<Vec<(usize, bool)>>) -> Option<Vec<&(usize, bool)>> {
    let completed_row = board
        .iter()
        .find(|&&row| completed(row.iter()))?
        .iter()
        .collect();
    Some(completed_row)
}

fn find_completed_col(board: &Vec<Vec<(usize, bool)>>) -> Option<Vec<&(usize, bool)>> {
    super::day3::get_columns(board)
        .find(|&col| completed(col))
        .map(Iterator::collect)
}

fn count_marked_spaces(board: &Vec<Vec<(usize, bool)>>) -> usize {
    let mut count = 0;
    for row in board {
        for &(value, marked) in row {
            if marked {
                count += 1;
            }
        }
    }
    return count;
}

fn completed<'a>(spaces: impl Iterator<Item = &'a (usize, bool)>) -> bool {
    spaces.fold(true, |acc, &(cell, marked)| acc && marked)
}
