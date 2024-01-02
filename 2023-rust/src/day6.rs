use std::iter::zip;

use nom::{
    bytes::complete::tag,
    character::complete::{digit1, multispace1, newline},
    combinator::{map, map_res},
    multi::{many0},
    sequence::{preceded, separated_pair},
    IResult,
};

fn read_numbers(input: &str) -> IResult<&str, Vec<u64>> {
    many0(preceded(multispace1, map_res(digit1, str::parse)))(input)
}

fn read_number(input: &str) -> IResult<&str, u64> {
    map_res(many0(preceded(multispace1, digit1)), |x| x.join("").parse())(input)
}

fn read_input(input: &str) -> IResult<&str, Vec<(u64, u64)>> {
    map(
        separated_pair(
            preceded(tag("Time:"), read_numbers),
            newline,
            preceded(tag("Distance:"), read_numbers),
        ),
        |(times, distances)| zip(times, distances).collect(),
    )(input)
}

fn read_input2(input: &str) -> IResult<&str, (u64, u64)> {
    separated_pair(
        preceded(tag("Time:"), read_number),
        newline,
        preceded(tag("Distance:"), read_number),
    )(input)
}

pub fn _part1(input: &str) -> usize {
    let (_leftovers, records) = read_input(input).unwrap();
    records
        .into_iter()
        .map(|(time, distance)| {
            (1..time)
                .map(|i| i * (time - i))
                .filter(|&x| x > distance)
                .count()
        })
        .product()
}

pub fn _part2(input: &str) -> usize {
    let (_leftovers, (time, distance)) = read_input2(input).unwrap();
    (1..time)
        .map(|i| i * (time - i))
        .filter(|&x| x > distance)
        .count()
}
