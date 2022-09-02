// https://adventofcode.com/2021/day/3
use crate::types::{Answer, NoSolutionError};
use anyhow::Result;

pub fn solve(input: String) -> Result<(Answer, Answer)> {
    let report = Report::from(input);
    let soln1 = report.part1().ok_or(NoSolutionError.into());
    let soln2 = report.part2().ok_or(NoSolutionError.into());
    Ok((soln1, soln2))
}

struct Report(Vec<Vec<bool>>);

impl From<String> for Report {
    fn from(input: String) -> Self {
        return Report(
            input
                .split("\n")
                .filter(|&s| s != "")
                .map(bitstring_to_bools)
                .collect::<Vec<_>>(),
        );
    }
}

impl Report {
    pub fn part1(&self) -> Option<i64> {
        let Report(rows) = self;

        let common_bits = get_columns(rows)
            .into_iter()
            .map(most_common_bit)
            .collect::<Vec<bool>>();
        let uncommon_bits = common_bits.into_iter().map(|b| !b).collect::<Vec<bool>>();

        let gamma_rate = greek_rate(common_bits);
        let epsilon_rate = greek_rate(uncommon_bits);

        Some(gamma_rate * epsilon_rate)
    }

    pub fn part2(&self) -> Option<i64> {
        let o2_rating = self.chem_rating(most_common_bit)?;
        let co2_rating = self.chem_rating(|x| !most_common_bit(x))?;

        return Some(o2_rating * co2_rating);
    }

    fn chem_rating<'a>(&self, rate_chem: fn(Vec<bool>) -> bool) -> Option<i64> {
        let Report(rows) = self;

        let row = get_columns(rows)
            .into_iter()
            .map(rate_chem)
            .enumerate()
            .find_map(|(col_index, mode)| rows.iter().find(|row| row[col_index] == mode))?
            .clone();

        Some(greek_rate(row))
    }

    fn row_len(&self) -> Option<usize> {
        self.0.first()?.len().into()
    }
}

fn get_columns<'a, T: Copy>(rows: &Vec<Vec<T>>) -> Vec<Vec<T>> {
    match rows.len() {
        0 => Vec::new(),
        row_len => (0..row_len)
            .map(|col_index| get_column(rows, col_index))
            .collect::<Vec<_>>(),
    }
}

pub fn get_column<'a, T>(rows: &Vec<Vec<T>>, col_index: usize) -> Vec<T> {
    rows.into_iter().map(|b| b[col_index]).collect()
}

fn most_common_bit(bits: Vec<bool>) -> bool {
    let ham_weight: usize = bits.iter().map(|&b| usize::from(b)).sum();
    return ham_weight >= bits.len() / 2;
}

fn greek_rate(row: Vec<bool>) -> i64 {
    let bitstring = row
        .iter()
        .map(|&b| i64::from(b).to_string())
        .collect::<Vec<String>>()
        .join("");

    return i64::from_str_radix(bitstring.as_str(), 2).unwrap();
}

fn bitstring_to_bools(string: &str) -> Vec<bool> {
    string.chars().map(|c| c == '1').collect()
}
