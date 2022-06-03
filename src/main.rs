#![feature(custom_test_frameworks)]
#![feature(iterator_try_reduce)]
#![feature(hash_drain_filter)]
#![feature(array_zip)]
#![feature(slice_as_chunks)]
#![feature(let_chains)]
use std::{env, time::Instant};

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
    ]
    .iter()
    .enumerate()
    .map(|(day, solver)| {
        day += 1; // Days start at 1.
        let input_path = format!("puzzle_input/{}{}.txt", filename, day);
        let timer = Instant::now();
        let result = solver(&input_path);
        print_results(day, result, timer.elapsed().as_millis());
    });
}

fn print_results(day: usize, result: Result<(Solution, Solution), Error>, time: u128) {
    print!("Day {} | ", day);
    print!("\tTime:   {}ms", time);
    match result {
        Ok((soln1, soln2)) => {
            print!("\tPart 1: {:?}, ", soln1);
            print!("\tPart 2: {:?}, ", soln2);
        }
        Err(e) => print!("\tEncountered an Error: {:?}, ", e),
    }
    println!("");
}
