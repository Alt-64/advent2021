use std::fs::read_to_string;

use crate::types::{Error, Solution};

pub fn solve(path: &str) -> Result<(Solution, Solution), Error> {
    let mut soln1 = 0;
    let mut autocomplete_scores = Vec::new();

    for line in read_to_string(path)?.split("\n").filter(|&s| s != "") {
        let mut stack = Vec::<char>::new();
        if let Some(bad_bracket) = has_bad_bracket(line, &mut stack) {
            soln1 += score_bad(bad_bracket);
        } else {
            let score = stack
                .into_iter()
                .rev()
                .fold(0, |acc, curr| acc * 5 + score_autocomplete(curr));
            autocomplete_scores.push(score);
        }
    }
    autocomplete_scores.sort();
    let soln2 = autocomplete_scores[autocomplete_scores.len() / 2];

    Ok((Ok(soln1), Ok(soln2)))
}

fn has_bad_bracket(line: &str, stack: &mut Vec<char>) -> Option<char> {
    line.chars().find(|&bracket| {
        if is_open(&bracket) {
            stack.push(bracket);
            return false;
        } else if let Some(left) = stack.pop() {
            return !pairs_with(left, bracket);
        } else {
            return false;
        }
    })
}

fn score_bad(bad_bracket: char) -> i64 {
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
    match (b1, b2) {
        ('(', ')') | ('[', ']') | ('{', '}') | ('<', '>') => true,
        _ => false,
    }
}

fn is_open(bracket: &char) -> bool {
    match bracket {
        '(' | '[' | '{' | '<' => true,
        ')' | ']' | '}' | '>' => false,
        _ => false,
    }
}
