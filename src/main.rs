#![feature(let_chains)]
#![feature(array_windows)]

use std::{
    env,
    fmt::Debug,
    fs::read_to_string,
    sync::mpsc,
    time::{Duration, Instant},
};

use anyhow::Result;
use types::Solver;

mod aoc2021;
mod types;

fn main() {
    use aoc2021::*;
    let puzzle_file_prefix = env::args().skip(1).next().unwrap_or("input".to_string());
    let (tx, rx) = mpsc::channel();

    let solvers: [fn(&str) -> Result<Solver>; 13] = [
        day1::Day1::try_from,
        day2::Day2::try_from,
        day3::Day3::try_from,
        day4::Day4::try_from,
        day5::Day5::try_from,
        day6::Day6::try_from,
        day7::Day7::try_from,
        day8::Day8::try_from,
        day9::Day9::try_from,
        day10::Day10::try_from,
        day11::Day11::try_from,
        day12::Day12::try_from,
        day13::Day13::try_from,
    ];
    let (tx, rx) = mpsc::channel();

    for (day, solver) in (1..=13).zip(solvers) {
        let path = format!("puzzle_input/{puzzle_file_prefix}_day{day}.txt");
        let input = read_to_string(path).unwrap().trim();
        let timer = Instant::now();
        let answer = solver.solve(input, tx.clone());
        let duration = timer.elapsed();
        // print_result(answer, duration, day)
    }
}

// fn print_result(answer: Result<(Answer, Answer)>, duration: Duration, day: usize) {
//     println!("Day {day} | {:2}ms", duration.as_micros() as f64 / 1000.0);
//     match answer {
//         Ok((soln1, soln2)) => {
//             println!("- 1: {:?} ", Box::new(soln1));
//             println!("- 2: {:?} ", Box::new(soln2));
//         }
//         Err(e) => println!("Encountered an error: {:?}", e),
//     }
//     println!("");
// }
