use std::cmp::max;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1, line_ending, space1},
    combinator::{eof, map_res, opt},
    multi::{many1, separated_list1},
    sequence::*,
    IResult,
};

type MarbleSet = (u8, u8, u8);

fn game_id(input: &str) -> IResult<&str, (&str, &str)> {
    separated_pair(tag("Game"), space1, digit1)(input)
}

fn into_marble_set(marble_counts: Vec<(u8, &str)>) -> Result<MarbleSet, ()> {
    marble_counts
        .into_iter()
        .try_fold((0, 0, 0), |acc, (count, color)| {
            match color.chars().next() {
                Some('r') => Ok((acc.0 + count, acc.1, acc.2)),
                Some('g') => Ok((acc.0, acc.1 + count, acc.2)),
                Some('b') => Ok((acc.0, acc.1, acc.2 + count)),
                _ => Err(()),
            }
        })
}

fn marble_count(input: &str) -> IResult<&str, (u8, &str)> {
    pair(
        delimited(space1, map_res(digit1, str::parse::<u8>), space1),
        alt((tag("red"), tag("green"), tag("blue"))),
    )(input)
}

fn round(input: &str) -> IResult<&str, MarbleSet> {
    map_res(separated_list1(char(','), marble_count), into_marble_set)(input)
}

fn game(input: &str) -> IResult<&str, Vec<MarbleSet>> {
    delimited(
        pair(game_id, char(':')),
        separated_list1(char(';'), round),
        opt(line_ending),
    )(input)
}

fn max_3tuple<T: Ord>(a: (T, T, T), b: (T, T, T)) -> (T, T, T) {
    (max(a.0, b.0), max(a.1, b.1), max(a.2, b.2))
}

pub fn _part1(input: &str) -> usize {
    let (_leftover, games) = terminated(many1(game), eof)(input).unwrap();
    games
        .into_iter()
        .enumerate()
        .filter_map(|(i, rounds)| {
            let (red, green, blue) = rounds.into_iter().fold((0, 0, 0), max_3tuple);
            (red <= 12 && green <= 13 && blue <= 14).then_some(i + 1)
        })
        .sum()
}

pub fn _part2(input: &str) -> u64 {
    let (_leftover, games) = many1(game)(input).unwrap();
    println!("{}", games.len());
    games
        .into_iter()
        .map(|rounds| {
            let (red, green, blue) = rounds.into_iter().fold((0, 0, 0), max_3tuple);
            (red as u64) * (green as u64) * (blue as u64)
        })
        .sum()
}
