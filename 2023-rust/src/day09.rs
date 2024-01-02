use std::fs::read_to_string;

use itertools::Itertools;
use nom::{
    character::complete::{self, newline, space1},
    multi::separated_list1,
    IResult,
};

fn read_input(input: &str) -> IResult<&str, Vec<Vec<i64>>> {
    separated_list1(newline, separated_list1(space1, complete::i64))(input)
}

pub fn part1() -> i64 {
    let input = read_to_string("input/9").unwrap();
    let (_leftover, histories) = read_input(&input).unwrap();
    histories
        .into_iter()
        .map(|mut xs| {
            let mut last_xs = vec![*xs.last().unwrap()];
            loop {
                xs = xs.windows(2).map(|x| x[1] - x[0]).collect_vec();
                last_xs.push(*xs.last().unwrap());
                if xs.iter().all_equal() {
                    break;
                }
            }
            last_xs.iter().sum::<i64>()
        })
        .sum()
}

pub fn part2() -> i64 {
    let input = read_to_string("input/9").unwrap();
    let (_leftover, histories) = read_input(&input).unwrap();
    histories
        .into_iter()
        .map(|mut xs| {
            let mut first_xs = vec![*xs.first().unwrap()];
            loop {
                xs = xs.windows(2).map(|x| x[1] - x[0]).collect();
                first_xs.push(*xs.first().unwrap());
                if xs.iter().all_equal() {
                    break;
                }
            }
            first_xs
                .into_iter()
                .rev()
                .reduce(|acc, cur| cur - acc)
                .unwrap()
        })
        .sum()
}
