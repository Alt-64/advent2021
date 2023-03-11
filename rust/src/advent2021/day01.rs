// https://adventofcode.com/2021/day/1

use std::{sync::mpsc::Sender, thread};

fn part_1(depths: &Vec<u16>) -> usize {
    depths
        .array_windows::<2>()
        .filter(|&[left, right]| left < right)
        .count()
}

fn part_2(depths: &Vec<u16>) -> usize {
    depths
        .array_windows::<4>()
        .filter(|&[left, _, _, right]| left < right)
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solve() {
        let depths: Vec<_> = include_str!("day01_input.txt")
            .trim()
            .split("\n")
            .map(str::parse::<u16>)
            .collect::<Result<_, _>>()
            .expect("failed to read input string");
        assert_eq!(part_1(&depths), 1559);
        assert_eq!(part_2(&depths), 1600);
    }
}
