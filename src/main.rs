use std::{fs::read_to_string, io::Error, num::ParseIntError};

mod day1;
mod day2;

fn main() {
    // day1("input_day1.txt").unwrap();
    let res = day2::solution("input_day2.txt").unwrap();
    println!("{}", res)
}
