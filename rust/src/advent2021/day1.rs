// https://adventofcode.com/2021/day/1

use std::{sync::mpsc::Sender, thread};

use crate::types::Solution;

pub fn solve(input: &str, tx: Sender<(usize, usize, Solution)>) -> anyhow::Result<()> {
    let depths: Vec<_> = input
        .split("\n")
        .map(str::parse::<u16>)
        .collect::<Result<_, _>>()?;

    let tx_1 = tx.clone();
    let handle = thread::spawn(move || tx_1.send((1, 1, Ok(Box::new(part_1(depths))))));
    tx.send((1, 1, Ok(Box::new(part_2(depths)))))?;

    handle.join().unwrap().map_err(Into::into)
}

fn part_1(depths: Vec<u16>) -> usize {
    depths
        .array_windows::<2>()
        .filter(|&[left, right]| left < right)
        .count()
}

fn part_2(depths: Vec<u16>) -> usize {
    depths
        .array_windows::<4>()
        .filter(|&[left, _, _, right]| left < right)
        .count()
}
