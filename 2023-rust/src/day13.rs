use std::{fmt::Binary, fs::read_to_string};

use itertools::Itertools;
use nom::{
    bytes::complete::is_a, character::complete::newline, multi::separated_list1, sequence::pair,
    IResult,
};

fn parse_patterns(input: &str) -> IResult<&str, Vec<Vec<&str>>> {
    separated_list1(pair(newline, newline), separated_list1(newline, is_a("#.")))(input)
}

fn transpose<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>>
where
    T: Clone,
{
    assert!(!v.is_empty());
    (0..v[0].len())
        .map(|i| v.iter().map(|inner| inner[i].clone()).collect::<Vec<T>>())
        .collect()
}

pub fn part1() -> usize {
    let input = read_to_string("input/13").unwrap();
    let (_leftover, patterns) = parse_patterns(&input).unwrap();

    let flipped_patterns = patterns.iter().map(|pattern| {
        transpose(
            pattern
                .iter()
                .map(|line| line.chars().collect_vec())
                .collect_vec(),
        )
        .into_iter()
        .map(String::from_iter)
        .map(|x| compress_line(&x))
        .collect_vec()
    });

    patterns
        .iter()
        .map(|x| x.iter().map(|x| compress_line(x)).collect_vec())
        .zip(flipped_patterns)
        .map(|(pattern, flipped_pattern)| {
            if let Some(mirror_idx) = find_mirror(&pattern, false) {
                mirror_idx * 100
            } else if let Some(mirror_idx) = find_mirror(&flipped_pattern, false) {
                mirror_idx
            } else {
                panic!()
            }
        })
        .sum()
}

fn printout<T: Binary>(pattern: &[T], a: usize, b: usize) {
    for (i, line) in pattern.iter().enumerate() {
        if i == a || i == b {
            print!(">");
        } else {
            print!(" ");
        }
        println!("{line:032b}");
    }
    println!();
}

fn find_mirror<T: Eq + Binary>(pattern: &[T], show: bool) -> Option<usize> {
    (1..pattern.len()).find(|&i| {
        (0..i).rev().zip(i..pattern.len()).all(|(above, below)| {
            if show {
                printout(pattern, above, below);
            }
            pattern[above] == pattern[below]
        })
    })
}

fn find_smudged_mirror(pattern: &[u32], show: bool) -> Option<usize> {
    (1..pattern.len()).find(|&i| {
        let mut fixed = false;
        (0..i).rev().zip(i..pattern.len()).all(|(above, below)| {
            if show {
                printout(pattern, above, below);
            }
            let diffs = (pattern[above] ^ pattern[below]).count_ones();
            if diffs == 1 && !fixed {
                fixed = true;
                true
            } else {
                diffs > 0
            }
        }) && fixed
    })
}

fn compress_line(line: &str) -> u32 {
    line.chars()
        .enumerate()
        .map(|(i, c)| match c {
            '#' => 1 << i,
            '.' => 0,
            _ => panic!(),
        })
        .sum()
}

pub fn part2() -> usize {
    let input = read_to_string("input/13").unwrap();
    let (_leftover, patterns) = parse_patterns(&input).unwrap();

    let flipped_patterns = patterns.iter().map(|pattern| {
        transpose(
            pattern
                .iter()
                .map(|line| line.chars().collect_vec())
                .collect_vec(),
        )
        .into_iter()
        .map(String::from_iter)
        .map(|x| compress_line(&x))
        .collect_vec()
    });

    let reg_patterns = patterns
        .iter()
        .map(|x| x.into_iter().map(|x| compress_line(x)).collect_vec());

    reg_patterns
        .zip(flipped_patterns)
        .map(|(pattern, flipped_pattern)| {
            if let Some(mirror_idx) = find_smudged_mirror(&pattern, false) {
                mirror_idx * 100
            } else if let Some(mirror_idx) = find_smudged_mirror(&flipped_pattern, false) {
                mirror_idx
            } else {
                panic!()
            }
        })
        .sum()
}
