use crate::types::Answer;
use anyhow::Result;
use itertools::{Either, Itertools};

pub fn solve(input: &str) -> Result<(Answer, Answer)> {
    let (completable_stacks, incompletable_brackets): (Vec<_>, Vec<_>) =
        input.split("\n").partition_map(is_completable);

    let soln1 = part1(incompletable_brackets);
    let soln2 = part2(completable_stacks);

    Ok((Ok(soln1), Ok(soln2)))
}

fn is_completable(line: &str) -> Either<Vec<char>, char> {
    let mut stack = Vec::<char>::new();
    line.chars()
        .find(|&bracket| is_bad_bracket(&mut stack, bracket))
        .ok_or(stack)
        .into()
}

fn part1(incompletable_brackets: Vec<char>) -> i64 {
    incompletable_brackets
        .into_iter()
        .map(score_incompletable)
        .sum()
}

fn part2(stacks: Vec<Vec<char>>) -> i64 {
    let mut scores: Vec<_> = stacks.into_iter().map(score_completable_stack).collect();
    scores.sort();
    return scores[scores.len() / 2];
}

fn score_completable_stack(stack: Vec<char>) -> i64 {
    let mut score = 0;
    for bracket in stack {
        score *= 5;
        score += score_autocomplete(bracket);
    }
    return score;
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

fn score_incompletable(bad_bracket: char) -> i64 {
    match bad_bracket {
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
