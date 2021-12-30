use types::{Error, Solution};

mod day1;
mod day2;
mod day3;
mod day4;
mod types;

fn main() {
    [day1::solver, day2::solver, day3::solver, day4::solver]
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
