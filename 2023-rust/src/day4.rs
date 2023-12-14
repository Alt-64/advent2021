use std::collections::HashSet;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, line_ending, multispace0, multispace1},
    combinator::{eof, map, map_res},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    IResult,
};

fn prefix(input: &str) -> IResult<&str, usize> {
    delimited(
        pair(tag("Card"), multispace1),
        map_res(digit1, str::parse::<usize>),
        nom::character::complete::char(':'),
    )(input)
}

fn numbers(input: &str) -> IResult<&str, HashSet<u8>> {
    map(
        many1(preceded(multispace0, map_res(digit1, str::parse::<u8>))),
        |xs| xs.into_iter().collect::<HashSet<_>>(),
    )(input)
}

pub fn _part1(input: &str) -> u32 {
    let (_leftover, scratch_cards) = separated_list1(
        line_ending,
        preceded(prefix, separated_pair(numbers, tag(" | "), numbers)),
    )(input)
    .unwrap();
    scratch_cards
        .into_iter()
        .flat_map(|(l, r)| {
            l.into_iter()
                .filter(|x| r.contains(x))
                .count()
                .checked_sub(1)
                .map(|y| 1 << y)
        })
        .sum()
}

pub fn _part2(input: &str) -> u32 {
    let (_leftover, scratch_cards) = separated_list1(
        line_ending,
        preceded(prefix, separated_pair(numbers, tag(" | "), numbers)),
    )(input)
    .unwrap();

    let mut copies: Vec<u32> = vec![0; scratch_cards.len()];

    scratch_cards
        .into_iter()
        .map(|(l, r)| l.into_iter().filter(|x| r.contains(x)).count())
        .enumerate()
        .for_each(|(i, wins)| {
            copies[i] += 1;
            for j in 0..wins {
                copies[i + j + 1] += copies[i]
            }
        });
    copies.into_iter().sum()
}
