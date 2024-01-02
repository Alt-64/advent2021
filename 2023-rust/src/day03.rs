use std::collections::HashMap;

use itertools::iproduct;

fn read1(input: &str) -> (HashMap<(usize, usize), char>, Vec<(usize, usize)>) {
    input
        .lines()
        .enumerate()
        .map(|(i, line)| line.chars().enumerate().map(move |(j, c)| ((i, j), c)))
        .flatten()
        .fold(
            (HashMap::new(), Vec::new()),
            |(mut acc_digits, mut acc_symbols), (coord, c)| {
                match c {
                    '.' => (),
                    x if '0' <= x && x <= '9' => {
                        acc_digits.insert(coord, c);
                    }
                    _ => {
                        acc_symbols.push(coord);
                    }
                }
                (acc_digits, acc_symbols)
            },
        )
}

fn read2(input: &str) -> (HashMap<(usize, usize), char>, Vec<(usize, usize)>) {
    input
        .lines()
        .enumerate()
        .map(|(i, line)| line.chars().enumerate().map(move |(j, c)| ((i, j), c)))
        .flatten()
        .fold(
            (HashMap::new(), Vec::new()),
            |(mut acc_digits, mut acc_symbols), (coord, c)| {
                match c {
                    '*' => {
                        acc_symbols.push(coord);
                    }
                    x if '0' <= x && x <= '9' => {
                        acc_digits.insert(coord, c);
                    }
                    _ => (),
                }
                (acc_digits, acc_symbols)
            },
        )
}

pub fn _part1(input: &str) -> u32 {
    let (mut values, symbols) = read1(input);

    symbols
        .iter()
        .map(|(x, y)| {
            let xs = x.saturating_sub(1)..=x.saturating_add(1);
            let ys = y.saturating_sub(1)..=y.saturating_add(1);
            let z = iproduct!(xs, ys)
                .flat_map(|(i, mut j)| {
                    let mut a = String::new();
                    if values.contains_key(&(i, j)) {
                        while j != 0 && values.contains_key(&(i, j - 1)) {
                            j -= 1;
                        }
                        while let Some(c) = values.remove(&(i, j)) {
                            a.push(c);
                            j += 1;
                        }
                        a.parse::<u32>().ok()
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            z
        })
        .flatten()
        .sum()
}

pub fn _part2(input: &str) -> u32 {
    let (mut values, symbols) = read2(input);

    symbols
        .iter()
        .map(|(x, y)| {
            let xs = x.saturating_sub(1)..=x.saturating_add(1);
            let ys = y.saturating_sub(1)..=y.saturating_add(1);
            let z = iproduct!(xs, ys)
                .flat_map(|(i, mut j)| {
                    let mut a = String::new();
                    if values.contains_key(&(i, j)) {
                        while j != 0 && values.contains_key(&(i, j - 1)) {
                            j -= 1;
                        }
                        while let Some(c) = values.remove(&(i, j)) {
                            a.push(c);
                            j += 1;
                        }
                        a.parse::<u32>().ok()
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            if z.len() == 2 {
                Some(z.into_iter().fold(1, |acc, cur| acc * cur))
            } else {
                None
            }
        })
        .flatten()
        .sum()
}
