#![feature(custom_test_frameworks)]
#![feature(iterator_try_reduce)]
#![feature(hash_drain_filter)]
#![feature(array_zip)]
#![feature(slice_as_chunks)]
#![feature(let_chains)]
use std::{
    env,
    fs::read_to_string,
    time::{Duration, Instant},
};

use anyhow::Result;
use types::{Answer, Error, Solution};

mod aoc2021;
mod types;

fn main() {
    let puzzle_file = env::args().next().unwrap_or("input".to_string());

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

    (1..12)
        .zip(solvers)
        .map(|(day, solver)| play_puzzle(day, puzzle_file, solver));
}

fn play_puzzle(day: usize, puzzle_file_prefix: String, solver: Solution) -> Result<()> {
    let path = format!("puzzle_input/{filename}_{day}.txt");
    let input = read_to_string(path)?;

    let timer = Instant::now();
    let answer = solver(input);
    let duration = timer.elapsed();
    print!("Day {day} | Duration {}µs", duration.as_micros());
    match answer {
        Ok((soln1, soln2)) => {
            print!("\tPart 1: {:?}, ", soln1);
            print!("\tPart 2: {:?}, ", soln2);
        }
        Err(e) => print!("\tEncountered an Error: {:?}, ", e),
    }
    println!("");

    Ok(())
}
