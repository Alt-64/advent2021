use std::fs::read_to_string;

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
    let view: Vec<&[bool]> = diagnostic_report.iter().map(Vec::as_slice).collect();

    if let Some(row) = view.first() {
        let common_bits: Vec<bool> = (0..row.len())
            .map(|i| get_column(&view, i))
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

fn get_column<T: Clone>(matrix: &Vec<&[T]>, column: usize) -> Vec<T> {
    matrix.iter().map(|b| b[column].clone()).collect()
}
fn most_common_bit(bits: &[bool]) -> bool {
    let ham_weight: usize = bits.iter().map(|&b| usize::from(b)).sum();
    return ham_weight >= bits.len() / 2;
}

fn calc_rate(row: &[bool]) -> i32 {
    let bitstring = row
        .iter()
        .map(|&b| i32::from(b))
        .map(|x| x.to_string())
        .collect::<Vec<String>>()
        .join("");

    return i32::from_str_radix(bitstring.as_str(), 2).unwrap();
}

fn calc_rating<'a>(mut bits: Vec<&[bool]>, calc_mode: &dyn Fn(&[bool]) -> bool) -> i32 {
    if let Some(bitstring) = bits.first() {
        for i in 0..bitstring.len() {
            let col = get_column(&bits, i);
            let mode = calc_mode(&col);
            bits = bits.into_iter().filter(|&row| row[i] == mode).collect();
            if bits.len() == 1 {
                break;
            }
        }
        calc_rate(bits[0])
    } else {
        0
    }
}

// ------------------------------------

pub fn part2(path: &str) -> Result<i32, Error> {
    let diagnostic_report = read_input(path)?;
    let view: Vec<&[bool]> = diagnostic_report.iter().map(Vec::as_slice).collect();

    let o2_rating = calc_rating(view.clone(), &|bits| most_common_bit(bits));
    let co2_rating = calc_rating(view, &|bits| !most_common_bit(bits));

    return Ok(o2_rating * co2_rating);
}
