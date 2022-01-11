#![feature(custom_test_frameworks)]
#![feature(iterator_try_reduce)]
use std::{env, os::unix::prelude::CommandExt};

use types::{Error, Solution};

mod aoc2021;
mod types;

fn main() {
    let filename = if env::args().skip(1).next().is_some() {
        "test_day"
    } else {
        "input_day"
    };
    [
        aoc2021::day1::solver,
        aoc2021::day2::solver,
        aoc2021::day3::solver,
        aoc2021::day4::solver,
        aoc2021::day5::solver,
        aoc2021::day6::solver,
        aoc2021::day7::solver,
    ]
    .iter()
    .enumerate()
    .map(|(day, solver)| {
        let day = day + 1;
        let input_path = format!("puzzle_input/{}{}.txt", filename, day);
        let result = solver(&input_path);
        print_results(day, result);
    })
    .for_each(drop);
}

fn print_results(day: usize, result: Result<(Solution, Solution), Error>) {
    println!("== Day {} ==", day);
    if let Ok((soln1, soln2)) = result {
        println!("Part 1: {:?}", soln1);
        println!("Part 2: {:?}", soln2);
    } else {
        println!("Encountered an Error: {:?}", result);
    }
    println!();
}
