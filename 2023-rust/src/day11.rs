use std::{
    cmp::{max, min},
    fs::read_to_string,
};

use itertools::Itertools;

fn solve(scale: usize) -> usize {
    let input = read_to_string("input/11")
        .unwrap()
        .lines()
        .map(|x| Vec::from(x))
        .collect_vec();
    let expanded_cols = (0..input.len())
        .filter(|col| input.iter().map(|row| row[*col]).all(|x| x == b'.'))
        .collect_vec();

    let mut exp_r = 0;
    let galaxies = input
        .iter()
        .enumerate()
        .map(|(i, x)| {
            let positions = x
                .iter()
                .enumerate()
                .filter_map(|(j, c)| {
                    let exp_c = expanded_cols.iter().filter(|&&col| col < j).count();
                    if *c == b'#' {
                        Some((j + exp_c * scale, i + exp_r * scale))
                    } else {
                        None
                    }
                })
                .collect_vec();
            if positions.len() == 0 {
                exp_r += 1;
            }
            positions
        })
        .flatten()
        .collect_vec();

    galaxies
        .iter()
        .enumerate()
        .map(|(i, (x, y))| {
            galaxies
                .iter()
                .skip(i + 1)
                .map(|(a, b)| max(b, y) - min(b, y) + max(x, a) - min(x, a))
                .sum::<usize>()
        })
        .sum::<usize>()
}

pub fn part1() -> usize {
    solve(1)
}
pub fn part2() -> usize {
    solve(999999)
}
