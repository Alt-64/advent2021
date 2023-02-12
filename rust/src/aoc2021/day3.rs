// https://adventofcode.com/2021/day/3
use crate::types::{BadInputError, NoSolutionError, SolveState, Solver};
use anyhow::Result;
use itertools::Itertools;
use std::fmt::Debug;

pub struct Day3 {
    state: SolveState,
    bit_matrix: Vec<Vec<bool>>,
}

impl TryFrom<&str> for Day3 {
    type Error = BadInputError;
    fn try_from(input: &str) -> Result<Day3, BadInputError> {
        Ok(Day3 {
            state: SolveState::new(),
            bit_matrix: input
                .split("\n")
                .map(bitstring_to_bools)
                .collect::<Result<_, _>>()?,
        })
    }
}

impl Solver<'_> for Day3 {
    type Soln1 = Result<i64>;
    fn solve_part1(&mut self) -> Self::Soln1 {
        let common_bits: Vec<_> = get_columns(&self.bit_matrix)
            .map(|column| get_mode(column.cloned().collect()))
            .collect();

        let epsilon_rate = greek_rate(common_bits.iter().map(|b| !b).collect());
        let gamma_rate = greek_rate(common_bits);

        Ok(epsilon_rate * gamma_rate)
    }

    type Soln2 = Result<i64>;
    fn solve_part2(&mut self) -> Self::Soln2 {
        let columns: Vec<Vec<bool>> = get_columns(&self.bit_matrix)
            .map(|column| column.cloned().collect())
            .collect();

        let o2_rating = columns
            .iter()
            .find_map(|&column| chem_rating(&self.bit_matrix, get_mode(column)))
            .ok_or(NoSolutionError)?;
        let co2_rating = columns
            .iter()
            .find_map(|&column| chem_rating(&self.bit_matrix, !get_mode(column)))
            .ok_or(NoSolutionError)?;

        Ok(o2_rating * co2_rating)
    }
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

fn chem_rating<'a>(bit_matrix: &Vec<Vec<bool>>, column_mode: bool) -> Option<i64> {
    let &row = bit_matrix.iter().find(|row| row.contains(&column_mode))?;
    Some(greek_rate(row))
}

impl Iterator for Day3 {
    type Item = Box<dyn Debug>;

    fn next(&mut self) -> Option<Box<dyn Debug>> {
        self.state.next()
    }
}
