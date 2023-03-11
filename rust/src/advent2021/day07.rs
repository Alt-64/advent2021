use std::thread;
use std::{num::ParseIntError, sync::mpsc::Sender};

use crate::types::{expect_soln, Solution};

fn solve(input: &str, tx: Sender<(usize, usize, Solution)>) -> anyhow::Result<()> {
    let crabs: Vec<_> = input
        .split(',')
        .map(|line| line.trim().parse::<u16>())
        .collect::<Result<_, _>>()?;

    fork_solve(&read_crabs(input)?, part_1, part_2, tx);
    let tx_1 = tx.clone();
    let handle = thread::spawn(move || tx_1.send((7, 1, expect_soln(part_1(&crabs)))));
    tx.send((7, 2, expect_soln(part_2(&crabs))))?;

    handle.join().unwrap().map_err(Into::into)
}

fn fork_solve<In: Send>(
    data: In,
    part_1: impl FnOnce(In) -> Solution,
    part_2: impl FnOnce(In) -> Solution,
    tx: Sender<(usize, usize, Solution)>,
) -> anyhow::Result<()> {
    let tx_1 = tx.clone();
    let handle = thread::spawn(move || tx_1.send((7, 1, part_1(data))));
    tx.send((7, 2, part_2(data)))?;

    handle.join().unwrap().map_err(Into::into)
}

fn read_crabs(input: &str) -> Result<Vec<u16>, ParseIntError> {
    input
        .split(',')
        .map(|line| line.trim().parse::<u16>())
        .collect::<Result<_, _>>()
}

fn part_1(crabs: &Vec<u16>) -> Option<u16> {
    let &median_crab = crabs.get(crabs.len() / 2)?;
    let distances_to_median_crab: u16 = crabs
        .iter()
        .map(|&crab| get_distance(crab, median_crab))
        .sum();
    Some(distances_to_median_crab)
}

fn part_2(crabs: &Vec<u16>) -> Option<u16> {
    let min = crabs.iter().min().cloned()?;
    let max = crabs.iter().max().cloned()?;

    let minimum_distance_to_align_crabs = (min..=max)
        .map(|pos| crabs.iter().map(|&crab| cost_to_move(crab, pos)).sum())
        .min();
    minimum_distance_to_align_crabs
}

fn get_distance(x: u16, y: u16) -> u16 {
    u16::abs_diff(x, y)
}

fn cost_to_move(crab: u16, dest: u16) -> u16 {
    fuel_cost(get_distance(crab, dest))
}

fn fuel_cost(dist: u16) -> u16 {
    dist.pow(2) - dist * (dist - 1) / 2
}

fn print_result(id: &str, result: Result<&str, &str>) -> String {
    let mid = if result.is_ok() {
        "\032[1;31mOk \033[0m"
    } else {
        "\031[1;31mErr\033[0m"
    };
    let end = unsafe { result.unwrap_unchecked() };

    [id, mid, end].join(" | ")
}
