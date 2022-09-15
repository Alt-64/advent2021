// https://adventofcode.com/2021/day/3
use crate::types::{Answer, NoSolutionError};
use anyhow::Result;

pub fn solve(input: &str) -> Result<(Answer, Answer)> {
    let report = read_input(input);
    let soln1 = part1(&report).ok_or(NoSolutionError.into());
    let soln2 = part2(&report).ok_or(NoSolutionError.into());
    Ok((soln1, soln2))
}

fn part1(report: &Vec<Vec<bool>>) -> Option<i64> {
    let (common_bits, uncommon_bits) = categorize_bits(report);

    let gamma_rate = greek_rate(&common_bits);
    let epsilon_rate = greek_rate(&uncommon_bits);

    Some(gamma_rate * epsilon_rate)
}

fn part2(report: &Vec<Vec<bool>>) -> Option<i64> {
    let o2_rating = chem_rating(report, most_common_bit)?;
    let co2_rating = chem_rating(report, |x| !most_common_bit(x))?;

    return Some(o2_rating * co2_rating);
}

fn categorize_bits(report: &Vec<Vec<bool>>) -> (Vec<bool>, Vec<bool>) {
    let common_bits = get_columns(report)
        .into_iter()
        .map(most_common_bit)
        .collect::<Vec<bool>>();
    let uncommon_bits = common_bits.iter().map(|b| !b).collect::<Vec<bool>>();

    return (common_bits, uncommon_bits);
}

fn most_common_bit(bits: Vec<bool>) -> bool {
    let ham_weight: usize = bits.iter().map(|&b| usize::from(b)).sum();
    return ham_weight >= bits.len() / 2;
}

fn greek_rate(row: &Vec<bool>) -> i64 {
    let bitstring = row
        .iter()
        .map(|&b| i64::from(b).to_string())
        .collect::<Vec<String>>()
        .join("");

    return i64::from_str_radix(bitstring.as_str(), 2).unwrap();
}

fn chem_rating<'a>(report: &Vec<Vec<bool>>, rate_chem: fn(Vec<bool>) -> bool) -> Option<i64> {
    get_columns(report)
        .into_iter()
        .enumerate()
        .find_map(|(i, col)| {
            let rating = rate_chem(col);
            return report.iter().find(|row| row[i] == rating).map(greek_rate);
        })
}

fn col_len<T>(rows: &Vec<Vec<T>>) -> usize {
    rows.first().map(|col| col.len()).unwrap_or(0).into()
}

fn get_columns<'a, T: Copy>(rows: &Vec<Vec<T>>) -> Vec<Vec<T>> {
    (0..col_len(rows))
        .map(|col_index| get_column(rows, col_index))
        .collect::<Vec<_>>()
}

pub fn get_column<'a, T: Copy>(rows: &Vec<Vec<T>>, col_index: usize) -> Vec<T> {
    rows.into_iter().map(|b| b[col_index]).collect()
}

fn bitstring_to_bools(string: &str) -> Vec<bool> {
    string.chars().map(|c| c == '1').collect()
}

fn read_input(input: &str) -> Vec<Vec<bool>> {
    input.split("\n").map(bitstring_to_bools).collect()
}
