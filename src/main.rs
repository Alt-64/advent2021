#![feature(custom_test_frameworks)]
use types::{Error, Solution};

mod aoc2021;
mod types;

fn main() {
    [
        aoc2021::day1::solver,
        aoc2021::day2::solver,
        aoc2021::day3::solver,
        aoc2021::day4::solver,
        aoc2021::day5::solver,
    ]
    .iter()
    .enumerate()
    .map(|(day, solver)| {
        let day = day + 1;
        let input_path = format!("puzzle_input/input_day{}.txt", day);
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
