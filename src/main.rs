#![feature(let_chains)]

use std::{
    env,
    fs::read_to_string,
    time::{Duration, Instant},
};

use anyhow::Result;
use types::{Answer, Solution};

mod aoc2021;
mod types;

fn main() {
    let puzzle_file_prefix = env::args().skip(1).next().unwrap_or("input".to_string());

    let solvers: [Solution; 12] = [
        aoc2021::day1::solve,
        aoc2021::day2::solve,
        aoc2021::day3::solve,
        aoc2021::day4::solve,
        aoc2021::day5::solve,
        aoc2021::day6::solve,
        aoc2021::day7::solve,
        aoc2021::day8::solve,
        aoc2021::day9::solve,
        aoc2021::day10::solve,
        aoc2021::day11::solve,
        aoc2021::day12::solve,
    ];

    for (day, solver) in (1..=12).zip(solvers) {
        let path = format!("puzzle_input/{puzzle_file_prefix}_day{day}.txt");
        let input = read_to_string(path).unwrap();
        let timer = Instant::now();
        let answer = solver(input.trim());
        let duration = timer.elapsed();
        print_result(answer, duration, day)
    }
}

fn print_result(answer: Result<(Answer, Answer)>, duration: Duration, day: usize) {
    println!("Day {day} | {}µs", duration.as_micros());
    match answer {
        Ok((soln1, soln2)) => {
            println!("- Part 1: {:?} ", soln1);
            println!("- Part 2: {:?} ", soln2);
        }
        Err(e) => println!("Encountered an error: {:?}", e),
    }
    println!("");
}
