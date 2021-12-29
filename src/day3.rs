use std::{fs::read_to_string, ops::Index};

use crate::errors::Error;

fn read_input(path: &str) -> Result<Vec<Vec<bool>>, Error> {
    let diagnostic_report: Vec<Vec<bool>> = read_to_string(path)?
        .split("\n")
        .filter(|&s| s != "")
        .map(bitstring_to_bools)
        .collect();
    Ok(diagnostic_report)
}

pub fn part1(path: &str) -> Result<i32, Error> {
    let diagnostic_report = read_input(path)?;

    if let Some(row) = diagnostic_report.first() {
        let common_bits: Vec<bool> = (0..row.len())
            .map(|i| get_column(diagnostic_report.iter(), i))
            .map(|col| most_common_bit(&col))
            .collect();

        let uncommon_bits = common_bits.iter().map(|b| !b).collect::<Vec<bool>>();

        let gamma_rate = calc_rate(&common_bits);
        let epsilon_rate = calc_rate(&uncommon_bits);

        Ok(gamma_rate * epsilon_rate)
    } else {
        Ok(0)
    }
}

fn bitstring_to_bools(string: &str) -> Vec<bool> {
    string.chars().map(|c| c == '1').collect()
}

pub fn get_column<'a, O, I, E>(rows: O, column_idx: usize) -> Vec<E>
where
    O: Iterator<Item = &'a I> + 'a,
    I: Index<usize, Output = E> + 'a + ?Sized,
    E: Clone,
{
    rows.map(|b| b[column_idx].clone()).collect()
}

fn most_common_bit(bits: &[bool]) -> bool {
    let ham_weight: usize = bits.iter().map(|&b| usize::from(b)).sum();
    return ham_weight >= bits.len() / 2;
}

fn calc_rate(row: &[bool]) -> i32 {
    let bitstring = row
        .iter()
        .map(|&b| i32::from(b).to_string())
        .collect::<Vec<String>>()
        .join("");

    return i32::from_str_radix(bitstring.as_str(), 2).unwrap();
}

fn calc_rating<'a>(report: &Vec<Vec<bool>>, calc_mode: &dyn Fn(&[bool]) -> bool) -> i32 {
    let mut rows = report.clone();
    if let Some(bitstring) = rows.first() {
        for i in 0..bitstring.len() {
            let col = get_column(rows.iter(), i);
            let mode = calc_mode(&col);
            rows = rows.into_iter().filter(|row| row[i] == mode).collect();
            if rows.len() == 1 {
                break;
            }
        }
        calc_rate(&rows[0])
    } else {
        0
    }
}

// ------------------------------------

pub fn part2(path: &str) -> Result<i32, Error> {
    let diagnostic_report = read_input(path)?;

    let o2_rating = calc_rating(&diagnostic_report, &|bits| most_common_bit(bits));
    let co2_rating = calc_rating(&diagnostic_report, &|bits| !most_common_bit(bits));

    return Ok(o2_rating * co2_rating);
}
