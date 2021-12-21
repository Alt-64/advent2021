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
    if let Some(bitstring) = diagnostic_report.first() {
        let n = bitstring.len();

        let common_bits: Vec<bool> = (0..n)
            .map(|i| {
                let col = get_col(&diagnostic_report, i);
                mode_bit(&col)
            })
            .collect();
        let uncommon_bits = common_bits.iter().map(|b| !b).collect();

        let gamma_rate = calc_rate(common_bits);
        let epsilon_rate = calc_rate(uncommon_bits);

        Ok(gamma_rate * epsilon_rate)
    } else {
        Ok(0)
    }
}

fn bitstring_to_bools(string: &str) -> Vec<bool> {
    string.chars().map(|c| c == '1').collect()
}

fn get_col<T: Clone>(matrix: &Vec<Vec<T>>, column: usize) -> Vec<T> {
    matrix.iter().map(|b| b[column].clone()).collect()
}

fn mode_bit(bits: &Vec<bool>) -> bool {
    let ham_weight: usize = bits.iter().map(|&b| usize::from(b)).sum();
    return ham_weight > bits.len() / 2;
}

fn calc_rate(bits: Vec<bool>) -> i32 {
    let bitstring = bits
        .iter()
        .map(|&b| i32::from(b))
        .map(|x| x.to_string())
        .collect::<Vec<String>>()
        .join("");

    return i32::from_str_radix(bitstring.as_str(), 2).unwrap();
}

// ------------------------------------

pub fn part2(path: &str) -> Result<i32, Error> {
    let diagnostic_report: Vec<Vec<bool>> = read_to_string(path)?
        .split("\n")
        .filter(|&s| s != "")
        .map(bitstring_to_bools)
        .collect();

    let col = get_col(&diagnostic_report, 0);

    Err(Error::Example)
}
