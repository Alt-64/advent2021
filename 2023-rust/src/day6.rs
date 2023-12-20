use std::iter::zip;

use nom::{IResult, sequence::{pair, preceded, separated_pair}, bytes::complete::tag, multi::{separated_list0, many0}, character::complete::{multispace0, digit1, newline, multispace1}, combinator::{map, map_res}};

fn read_numbers(input: &str) -> IResult<&str, Vec<u64>>{
    many0(preceded(multispace1, map_res(digit1, str::parse)))(input)
}

fn read_number(input: &str) -> IResult<&str, u64> {
    map_res(many0(preceded(multispace1, digit1)), |x| x.join("").parse())(input)
}

fn read_input(input: &str) -> IResult<&str, Vec<(u64, u64)>> {
    map(separated_pair(
        preceded(tag("Time:"), read_numbers),
        newline,
        preceded(tag("Distance:"), read_numbers),
    ), |(times, distances)| zip(times, distances).collect())(input)
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
    records.into_iter().map(|(time, distance)| {
        (1..time).map(|i| i * (time - i)).filter(|&x| x > distance).count() 
    }).product()
}

pub fn _part2(input: &str) -> usize {
    let (_leftovers, (time, distance)) = read_input2(input).unwrap();
    (1..time).map(|i| i * (time - i)).filter(|&x| x > distance).count() 
}
