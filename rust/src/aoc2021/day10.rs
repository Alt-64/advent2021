use itertools::{Either, Itertools};
use std::{sync::mpsc::Sender, thread};

use crate::types::Solution;

fn solve(input: &str, tx: Sender<(usize, usize, Solution)>) -> anyhow::Result<()> {
    let (completable_stacks, incompletable_brackets): (Vec<_>, Vec<_>) =
        input.split("\n").partition_map(is_completable);

    let tx_1 = tx.clone();
    let handle = thread::spawn(move || {
        let soln_1 = incompletable_brackets
            .into_iter()
            .map(score_incompletable)
            .sum();
        tx_1.send((10, 1, Ok(Box::new(soln_1))))
    });

    let mut scores: Vec<_> = completable_stacks.into_iter().map(score_stack).collect();
    scores.sort();
    let soln_2 = scores[scores.len() / 2];
    tx.send((10, 2, Ok(Box::new(soln_2))));

    handle.join().unwrap().map_err(Into::into)
}

fn is_completable(line: &str) -> Either<Vec<char>, char> {
    let mut stack = Vec::<char>::new();
    line.chars()
        .find(|&bracket| is_bad_bracket(&mut stack, bracket))
        .ok_or(stack)
        .into()
}

fn score_stack(stack: Vec<char>) -> u16 {
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

fn score_incompletable(bracket: char) -> u16 {
    match bracket {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => 0,
    }
}

fn score_autocomplete(bracket: char) -> u16 {
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
