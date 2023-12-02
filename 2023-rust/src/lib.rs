use regex::Regex;
use anyhow::Result;
use thiserror::*;

#[derive(Error, Debug)]
#[error("{0}")]
struct AdventError(String);


fn _part1(input: &str) -> u32 {
    let mut sum = 0;
    for line in input.split('\n') {
        let first = line.chars().filter(|c| c.is_ascii_digit()).nth(0);
        let reversed = line.chars().rev().collect::<String>();
        let last = reversed.chars().filter(|c| c.is_ascii_digit()).nth(0);
        let x = format!("{}{}", first.unwrap(), last.unwrap());
        sum += x.parse::<u32>().unwrap();
    }
    sum
}

fn _part2(input: &str) -> Result<u32> {
    let re_left = Regex::new(r"(one)|(two)|(three)|(four)|(five)|(six)|(seven)|(eight)|(nine)|(\d)")?;
    let re_right = Regex::new(r"(eno)|(owt)|(eerht)|(ruof)|(evif)|(xis)|(neves)|(thgie)|(enin)|(\d)")?;

    input.split('\n').map(|line| {
        let first = match re_left.captures(line).ok_or(AdventError("No capture".to_string()))?.get(0).unwrap().as_str() {
                "one" => '1',
                "two" => '2',
                "three" => '3',
                "four" => '4',
                "five" => '5', 
                "six" => '6',
                "seven" => '7',
                "eight" => '8',
                "nine" => '9',
                x => x.chars().nth(0).unwrap(),
        };

        let reversed = line.chars().rev().collect::<String>();
        let last = match re_right.captures(&reversed).ok_or(AdventError("No capture".to_string()))?.get(0).unwrap().as_str() { 
                "eno" => '1',
                "owt" => '2',
                "eerht" => '3',
                "ruof" => '4',
                "evif" => '5', 
                "xis" => '6',
                "neves" => '7',
                "thgie" => '8',
                "enin" => '9',
                x => x.chars().nth(0).unwrap(),
        };

        format!("{}{}", first, last).parse::<u32>().map_err(Into::into)
    }).sum()
}

#[cfg(test)]
mod tests {
    use std::fs::read_to_string;

    use crate::*;

    #[test]
    fn day1() {
        let input = read_to_string("puzzle_input/input-1").unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 54331);
        let soln2 = _part2(&input).unwrap();
        assert_eq!(soln2, 54518);
    }
}