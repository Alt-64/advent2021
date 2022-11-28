// https://adventofcode.com/2021/day/4
use std::{convert::TryFrom, num::ParseIntError};

use super::day3::get_column;

use anyhow::Result;

use crate::types::{BadInputError, NoSolutionError, Solver};

struct Day4(Bingo);

impl Solver for Day4 {
    type Soln1 = usize;
    fn solve_part1(&self) -> Result<Self::Soln1> {
        let (board, draw) = (&self.0).next().ok_or(NoSolutionError)?;
        Ok(draw * board.count_marked_spaces())
    }
    type Soln2 = usize;
    fn solve_part2(&self) -> Result<Self::Soln2> {
        let (board, draw) = self.0.last().ok_or(NoSolutionError)?;
        Ok(draw * board.count_marked_spaces())
    }
}

impl TryFrom<&str> for Day4 {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut inputs = value.split("\n\n");
        let first_line = inputs.next().ok_or(BadInputError("No line".to_string()))?;

        let draws = Box::new(read_input_draws(first_line)?.into_iter());
        let remaining_boards = inputs
            .map(BingoBoard::try_from)
            .collect::<Result<Vec<_>, _>>()?;

        let bingo = Bingo {
            curr_draw: usize::MAX,
            draws,
            remaining_boards,
            winning_boards: Box::new(Vec::new().into_iter()),
        };

        bingo.play_round().ok_or(NoSolutionError)?;

        Ok(Day4(bingo))
    }
}

fn read_input_draws(line: &str) -> Result<Vec<usize>, ParseIntError> {
    line.split(',').map(str::parse).collect()
}

fn read_input_board_line(line: &str) -> Result<Vec<BingoSpace>, ParseIntError> {
    line.split_whitespace().map(BingoSpace::try_from).collect()
}

#[derive(Clone, Copy, Debug)]
struct BingoSpace {
    value: usize,
    marked: bool,
}

impl BingoSpace {
    fn new(value: usize) -> Self {
        BingoSpace {
            value,
            marked: false,
        }
    }
}

impl TryFrom<&str> for BingoSpace {
    type Error = ParseIntError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse().map(BingoSpace::new)
    }
}

struct BingoBoard(Vec<Vec<BingoSpace>>);
impl BingoBoard {
    fn mark(&mut self, x: usize) {
        for row in self.0 {
            for space in row {
                if space.value == x {
                    space.marked = true;
                    return;
                }
            }
        }
    }

    fn get_completed_row(&self) -> Option<Vec<BingoSpace>> {
        self.0.iter().find(|&row| completed(row)).cloned()
    }

    fn get_completed_col(&self) -> Option<Vec<BingoSpace>> {
        self.0
            .first()?
            .iter()
            .enumerate()
            .map(|(i, _)| get_column(&self.0, i))
            .find(|col| completed(col))
    }

    fn has_won(&self) -> bool {
        self.find_completed_set().is_some()
    }

    fn find_completed_set(&self) -> Option<Vec<BingoSpace>> {
        self.get_completed_row()
            .or_else(|| self.get_completed_col())
    }

    fn count_marked_spaces(&self) -> usize {
        let mut count = 0;
        for row in self.0 {
            for space in row {
                if space.marked {
                    count += 1;
                }
            }
        }
        return count;
    }
}

impl TryFrom<&str> for BingoBoard {
    type Error = ParseIntError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value
            .split('\n')
            .map(read_input_board_line)
            .collect::<Result<_, _>>()
            .map(|board| BingoBoard(board))
    }
}

fn completed(spaces: &[BingoSpace]) -> bool {
    spaces.iter().fold(true, |acc, space| acc && space.marked)
}

struct Bingo {
    remaining_boards: Vec<BingoBoard>,
    winning_boards: Box<dyn Iterator<Item = BingoBoard>>,
    curr_draw: usize,
    draws: Box<dyn Iterator<Item = usize>>,
}

impl Bingo {
    fn play_round(&mut self) -> Option<()> {
        self.curr_draw = self.draws.next()?;

        for board in self.remaining_boards {
            board.mark(self.curr_draw);
        }

        let (remaining_boards, winning_boards) = self
            .remaining_boards
            .into_iter()
            .partition(|board| board.has_won());

        self.remaining_boards = remaining_boards;
        self.winning_boards = Box::new(winning_boards.into_iter());

        return Some(());
    }
}

impl<'a> Iterator for &'a Bingo {
    type Item = (&'a BingoBoard, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(board) = self.winning_boards.next() {
            return Some((&board, self.curr_draw));
        } else if self.play_round().is_some() {
            return self.next();
        } else {
            return None;
        }
    }
}
