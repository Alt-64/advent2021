use itertools::{Either, Itertools};
use std::fmt::Debug;

use crate::types::{BadInputError, SolveState, Solver};

struct Day10 {
    state: SolveState,
    completable_stacks: Vec<Vec<char>>,
    incompletable_brackets: Vec<char>,
}

impl Solver<'_> for Day10 {
    type Soln1 = i64;
    fn solve_part1(&mut self) -> Self::Soln1 {
        self.incompletable_brackets
            .into_iter()
            .map(score_incompletable)
            .sum()
    }

    type Soln2 = i64;
    fn solve_part2(&mut self) -> Self::Soln2 {
        let mut scores: Vec<_> = self
            .completable_stacks
            .into_iter()
            .map(score_stack)
            .collect();
        scores.sort();
        return scores[scores.len() / 2];
    }
}

impl TryFrom<&str> for Day10 {
    type Error = BadInputError;

    fn try_from(value: &str) -> std::result::Result<Self, Self::Error> {
        let (completable_stacks, incompletable_brackets): (Vec<_>, Vec<_>) =
            value.split("\n").partition_map(is_completable);

        Ok(Day10 {
            state: SolveState::new(),
            completable_stacks,
            incompletable_brackets,
        })
    }
}

fn is_completable(line: &str) -> Either<Vec<char>, char> {
    let mut stack = Vec::<char>::new();
    line.chars()
        .find(|&bracket| is_bad_bracket(&mut stack, bracket))
        .ok_or(stack)
        .into()
}

fn score_stack(stack: Vec<char>) -> i64 {
    stack
        .into_iter()
        .fold(0, |score, cur| score * 5 + score_autocomplete(cur))
}

fn is_bad_bracket(stack: &mut Vec<char>, bracket: char) -> bool {
    if is_open(&bracket) {
        stack.push(bracket);
        return false;
    } else if let Some(left) = stack.pop() {
        return !pairs_with(left, bracket);
    } else {
        return false;
    }
}

fn score_incompletable(bracket: char) -> i64 {
    match bracket {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => 0,
    }
}

fn score_autocomplete(bracket: char) -> i64 {
    match bracket {
        '(' => 1,
        '[' => 2,
        '{' => 3,
        '<' => 4,
        _ => 0,
    }
}

fn pairs_with(b1: char, b2: char) -> bool {
    matches!((b1, b2), ('(', ')') | ('[', ']') | ('{', '}') | ('<', '>'))
}

fn is_open(bracket: &char) -> bool {
    matches!(bracket, '(' | '[' | '{' | '<')
}

impl Iterator for Day10 {
    type Item = Box<dyn Debug>;

    fn next(&mut self) -> Option<Self::Item> {
        self.state.next()
    }
}
