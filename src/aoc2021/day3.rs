// https://adventofcode.com/2021/day/3
use crate::types::{NoSolutionError, Solver};
use anyhow::Result;
use itertools::Itertools;

pub struct Day3(Vec<Vec<bool>>);

impl Solver for Day3 {
    type Soln1 = i64;
    fn solve_part1(&self) -> Result<i64> {
        let (common_bits, uncommon_bits) = categorize_bits(&self.0);

        let gamma_rate = greek_rate(common_bits);
        let epsilon_rate = greek_rate(uncommon_bits);

        Ok(gamma_rate * epsilon_rate)
    }

    type Soln2 = i64;
    fn solve_part2(&self) -> Result<i64> {
        let o2_rating = chem_rating(&self.0, most_common_bit).ok_or(NoSolutionError)?;
        let co2_rating = chem_rating(&self.0, |x| !most_common_bit(x)).ok_or(NoSolutionError)?;

        Ok(o2_rating * co2_rating)
    }
}

impl From<&str> for Day3 {
    fn from(input: &str) -> Day3 {
        Day3(input.split("\n").map(bitstring_to_bools).collect())
    }
}

fn bitstring_to_bools(string: &str) -> Vec<bool> {
    string.chars().map(|c| c == '1').collect()
}

fn categorize_bits(
    report: &Vec<Vec<bool>>,
) -> (impl Iterator<Item = bool>, impl Iterator<Item = bool>) {
    let common_bits = get_columns(report).into_iter().map(most_common_bit);
    let uncommon_bits = common_bits.clone().map(|b| !b);

    return (common_bits, uncommon_bits);
}

fn get_columns<'a, T: Copy>(rows: &Vec<Vec<T>>) -> Vec<Vec<T>> {
    (0..col_len(rows))
        .map(|col_index| get_column(rows, col_index))
        .collect::<Vec<_>>()
}

fn col_len<T>(rows: &Vec<Vec<T>>) -> usize {
    rows.first().map(|col| col.len()).unwrap_or(0).into()
}

pub fn get_column<'a, T: Copy>(rows: &Vec<Vec<T>>, col_index: usize) -> Vec<T> {
    rows.into_iter().map(|b| b[col_index]).collect()
}

fn most_common_bit(bits: Vec<bool>) -> bool {
    let ham_weight: usize = bits.iter().map(|&b| usize::from(b)).sum();
    return ham_weight >= bits.len() / 2;
}

fn greek_rate(i: impl Iterator<Item = bool>) -> i64 {
    let bitstring = i.map(|b| i64::from(b).to_string()).join("");

    return i64::from_str_radix(bitstring.as_str(), 2).unwrap();
}

fn chem_rating<'a>(report: &Vec<Vec<bool>>, rate_chem: fn(Vec<bool>) -> bool) -> Option<i64> {
    for (i, col) in get_columns(report).into_iter().enumerate() {
        let rating = rate_chem(col);
        for row in report {
            if row.contains(&rating) {
                return Some(greek_rate(row.iter().cloned()));
            }
        }
    }
    None
}
