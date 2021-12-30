use std::{fs::read_to_string, ops::Index};

use crate::types::{Error, Solution};

pub fn solver(path: &str) -> Result<(Solution, Solution), Error> {
    let report = read_input(path)?;
    Ok((part1(&report), part2(&report)))
}

fn read_input(path: &str) -> Result<Vec<Vec<bool>>, Error> {
    let diagnostic_report: Vec<Vec<bool>> = read_to_string(path)?
        .split("\n")
        .filter(|&s| s != "")
        .map(bitstring_to_bools)
        .collect();
    Ok(diagnostic_report)
}

fn bitstring_to_bools(string: &str) -> Vec<bool> {
    string.chars().map(|c| c == '1').collect()
}

pub fn part1(report: &Vec<Vec<bool>>) -> Result<i32, Error> {
    if let Some(row) = report.first() {
        let common_bits: Vec<bool> = (0..row.len())
            .map(|i| get_column(report.iter(), i))
            .map(|col| most_common_bit(&col))
            .collect();

        let uncommon_bits = common_bits.iter().map(|b| !b).collect::<Vec<bool>>();

        let gamma_rate = greek_rate(&common_bits);
        let epsilon_rate = greek_rate(&uncommon_bits);

        Ok(gamma_rate * epsilon_rate)
    } else {
        Ok(0)
    }
}

pub fn part2(report: &Vec<Vec<bool>>) -> Result<i32, Error> {
    let o2_rating = chem_rating(report, &|col| most_common_bit(col));
    let co2_rating = chem_rating(report, &|col| !most_common_bit(col));

    return Ok(o2_rating * co2_rating);
}

pub fn get_column<'a, O, I, E>(rows: O, column_idx: usize) -> Vec<E>
where
    O: Iterator<Item = &'a I> + 'a,            // Outer Collection (rows)
    I: Index<usize, Output = E> + 'a + ?Sized, // Inner Collection (elements in row)
    E: Copy,                                   // Element type
{
    rows.map(|b| b[column_idx]).collect()
}

fn most_common_bit(bits: &[bool]) -> bool {
    let ham_weight: usize = bits.iter().map(|&b| usize::from(b)).sum();
    return ham_weight >= bits.len() / 2;
}

fn greek_rate(row: &[bool]) -> i32 {
    let bitstring = row
        .iter()
        .map(|&b| i32::from(b).to_string())
        .collect::<Vec<String>>()
        .join("");

    return i32::from_str_radix(bitstring.as_str(), 2).unwrap();
}

fn chem_rating<'a>(report: &Vec<Vec<bool>>, calc_mode: &dyn Fn(&[bool]) -> bool) -> i32 {
    let mut rows: Vec<&[bool]> = report.iter().map(Vec::as_slice).collect();
    if let Some(row) = rows.first() {
        for i in 0..row.len() {
            let col = get_column(rows.clone().into_iter(), i);
            let mode = calc_mode(&col);
            rows = rows.into_iter().filter(|row| row[i] == mode).collect();
            if rows.len() == 1 {
                break;
            }
        }
        greek_rate(&rows[0])
    } else {
        0
    }
}

// ------------------------------------
