use std::{
    collections::{HashMap, HashSet},
    fs::read_to_string,
};

use nom::{
    bytes::complete::tag,
    character::complete::{self, alpha1, newline},
    combinator::map,
    multi::{many1, separated_list1},
    sequence::{delimited, separated_pair},
    IResult,
};
use num::integer::lcm;

fn read_node(input: &str) -> IResult<&str, (&str, (&str, &str))> {
    separated_pair(
        alpha1,
        tag(" = "),
        delimited(
            complete::char('('),
            separated_pair(alpha1, tag(", "), alpha1),
            complete::char(')'),
        ),
    )(input)
}

fn read_input(input: &str) -> IResult<&str, (&str, HashMap<&str, (&str, &str)>)> {
    separated_pair(
        alpha1,
        many1(newline),
        map(separated_list1(newline, read_node), |nodes| {
            nodes.into_iter().collect()
        }),
    )(input)
}

pub fn _part1() -> usize {
    let input = read_to_string("input/8").unwrap();
    let (_leftover, (directions, nodes)) = read_input(&input).unwrap();

    let mut position = "AAA";
    let mut steps = 0;
    for direction in directions.chars().cycle() {
        steps += 1;
        let (left_path, right_path) = nodes.get(position).unwrap();
        position = if direction == 'L' {
            left_path
        } else {
            right_path
        };
        if position == "ZZZ" {
            break;
        }
    }

    return steps;
}

pub fn _part2() -> usize {
    let input = read_to_string("input/8").unwrap();
    let (_leftover, (directions, nodes)) = read_input(&input).unwrap();

    let positions: Vec<_> = nodes
        .iter()
        .filter_map(|(node, _)| {
            if node.chars().last()? == 'A' {
                Some(node)
            } else {
                None
            }
        })
        .collect();

    positions
        .into_iter()
        .map(|mut position| {
            let mut step = 0;
            for direction in directions.chars().cycle() {
                step += 1;
                let (left_path, right_path) = nodes.get(position).unwrap();
                position = if direction == 'L' {
                    left_path
                } else {
                    right_path
                };
                if position.chars().last().unwrap() == 'Z' {
                    break;
                }
            }
            return step;
        })
        .fold(1, lcm)
}
