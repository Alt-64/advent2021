#![feature(let_chains)]
#![feature(array_windows)]
#![feature(trait_alias)]

use std::{env, fs::read_to_string, time::Instant};

use std::fmt::Debug;
use types::Solver;

mod aoc2021;
mod types;

fn main() {
    use aoc2021::*;
    let puzzle_file_prefix = env::args().skip(1).next().unwrap_or("input".to_string());

    let x: [fn(&str) -> Result<Solver>; 1] = [day1::Day1::try_from, day2::Day2::try_from];

    let solvers: [Box<dyn Iterator<Item = Box<dyn Debug>>>; 1] = [
        Box::new(read_to_string("puzzle_input/day1.txt").and_then(day1::Day1::try_from)?),
        // read_to_string("puzzle_input/day2.txt").and_then(day2::Day2::try_from)?,
        // read_to_string("puzzle_input/day3.txt").and_then(day3::Day3::try_from)?,
        // read_to_string("puzzle_input/day4.txt").and_then(day4::Day4::try_from)?,
        // read_to_string("puzzle_input/day5.txt").and_then(day5::Day5::try_from)?,
        // read_to_string("puzzle_input/day6.txt").and_then(day6::Day6::try_from)?,
        // read_to_string("puzzle_input/day7.txt").and_then(day7::Day7::try_from)?,
        // read_to_string("puzzle_input/day8.txt").and_then(day8::Day8::try_from)?,
        // read_to_string("puzzle_input/day9.txt").and_then(day9::Day9::try_from)?,
        // read_to_string("puzzle_input/day10.txt").and_then(day10::Day10::try_from)?,
        // read_to_string("puzzle_input/day11.txt").and_then(day11::Day11::try_from)?,
        // read_to_string("puzzle_input/day12.txt").and_then(day12::Day12::try_from)?,
        // read_to_string("puzzle_input/day13.txt").and_then(day13::Day13::try_from)?,
    ];

    for solver in solvers {
        let timer = Instant::now();
        let soln1 = solver.solve_part1();
        let duration1 = timer.elapsed();
        let soln2 = solver.solve_part1();
        let duration2 = timer.elapsed();
        println!("- 1: {:?} - {:2}ms", soln1, duration1.as_millis());
        println!("- 2: {:?} - {:2}ms", soln2, duration2.as_millis());
    }
}
