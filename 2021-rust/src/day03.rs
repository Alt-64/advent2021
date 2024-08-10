// https://adventofcode.com/2021/day/3
use crate::types::{expect_soln, BadInputError, Solution};
use itertools::Itertools;
use std::{sync::mpsc::Sender, thread};

struct Day03;

fn solve(input: &str, tx: Sender<(usize, usize, Solution)>) -> anyhow::Result<()> {
    let bit_matrix: Vec<_> = input
        .split("\n")
        .map(bitstring_to_bools)
        .collect::<Result<_, _>>()?;

    let tx_1 = tx.clone();
    let handle = thread::spawn(move || tx.send((1, 1, expect_soln(part_1(&bit_matrix)))));
    tx.send((1, 2, expect_soln(part_2(&bit_matrix))))?;

    handle.join().unwrap().map_err(Into::into)
}

fn part_1(bit_matrix: &Vec<Vec<bool>>) -> Option<i64> {
    let common_bits: Vec<_> = get_columns(&bit_matrix)
        .map(|column| get_mode(column.cloned().collect()))
        .collect();

    let epsilon_rate = greek_rate(common_bits.iter().map(|b| !b).collect());
    let gamma_rate = greek_rate(common_bits);
    Some(epsilon_rate * gamma_rate)
}

fn part_2(bit_matrix: &Vec<Vec<bool>>) -> Option<i64> {
    let columns: Vec<Vec<bool>> = get_columns(&bit_matrix)
        .map(|column| column.cloned().collect())
        .collect();
    let o2_rating = columns
        .iter()
        .find_map(|&column| chem_rating(&bit_matrix, get_mode(column)))?;
    let co2_rating = columns
        .iter()
        .find_map(|&column| chem_rating(&bit_matrix, !get_mode(column)))?;
    Some(o2_rating * co2_rating)
}

fn bitstring_to_bools(string: &str) -> Result<Vec<bool>, BadInputError> {
    string
        .chars()
        .map(|c| match c {
            '1' => Ok(true),
            '0' => Ok(false),
            _ => Err(BadInputError(c.to_string())),
        })
        .collect()
}

pub fn get_columns<'a, T: Copy>(
    bit_matrix: &'a Vec<Vec<T>>,
) -> impl Iterator<Item = impl Iterator<Item = &'a T>> {
    let column_len = bit_matrix.first().map(Vec::len).unwrap_or(0);
    (0..column_len).map(|column_index| {
        bit_matrix
            .iter()
            .map(move |b| b.get(column_index))
            .flatten()
    })
}

fn get_mode(bits: Vec<bool>) -> bool {
    let weight = bits.iter().map(|&b| usize::from(b)).sum::<usize>();
    return weight >= bits.len() / 2;
}

fn greek_rate(i: Vec<bool>) -> i64 {
    let bitstring = i.into_iter().map(|b| i64::from(b).to_string()).join("");
    return i64::from_str_radix(bitstring.as_str(), 2).unwrap();
}

fn chem_rating<'a>(bit_matrix: &Vec<Vec<bool>>, mode: bool) -> Option<i64> {
    let &row = bit_matrix.iter().find(|row| row.contains(&mode))?;
    Some(greek_rate(row))
}
