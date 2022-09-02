// https://adventofcode.com/2021/day/3
use crate::types::{Answer, NoSolutionError};
use anyhow::Result;

pub fn solve(input: String) -> Result<(Answer, Answer)> {
    let report = Report::from(input);
    let soln1 = report.part1().ok_or(NoSolutionError.into());
    let soln2 = report.part2();
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

        let gamma_rate = greek_rate(&common_bits);
        let epsilon_rate = greek_rate(&uncommon_bits);

        Some(gamma_rate * epsilon_rate)
    }

    pub fn part2(&self) -> Result<i64> {
        let o2_rating = self.chem_rating(most_common_bit).unwrap_or(0);
        let co2_rating = self.chem_rating(most_common_bit).unwrap_or(0);

        return Ok(o2_rating * co2_rating);
    }

    fn chem_rating<'a>(&self, calc_mode: fn(Vec<bool>) -> bool) -> Option<i64> {
        let Report(rows) = self;
        let row_len = self.row_len()?;

        get_columns(rows).into_iter().find_map(|col| {
            let mode = calc_mode(col);
            let y = rows.into_iter().find(|x| x[col_index] == mode)?;
            Some(greek_rate(y))
        })
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

fn greek_rate(row: &[bool]) -> i64 {
    let bitstring = row
        .iter()
        .map(|&b| i64::from(b).to_string())
        .collect::<Vec<String>>()
        .join("");

    return i64::from_str_radix(bitstring.as_str(), 2).unwrap();
}

fn read_input(input: &str) -> Result<Vec<Vec<bool>>> {}

fn bitstring_to_bools(string: &str) -> Vec<bool> {
    string.chars().map(|c| c == '1').collect()
}
