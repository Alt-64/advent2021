use crate::types::{BadInputError, SolveState, Solver};
use anyhow::Result;
use itertools::Itertools;
use std::fmt::Debug;

type Segments = usize;

struct Day8 {
    state: SolveState,
    readouts: Vec<String>,
}

impl Solver<'_> for Day8 {
    type Soln1 = i64;
    fn solve_part1(&mut self) -> Self::Soln1 {
        self.readouts
            .join("")
            .chars()
            .filter(|digit| ['1', '4', '7', '8'].contains(digit))
            .count() as i64
    }

    type Soln2 = i64;
    fn solve_part2(&mut self) -> Self::Soln2 {
        self.readouts
            .iter()
            .map(|x| x.parse::<i64>().unwrap())
            .sum()
    }
}

impl TryFrom<&str> for Day8 {
    type Error = BadInputError;

    fn try_from(input: &str) -> std::result::Result<Self, Self::Error> {
        let readouts = input.split('\n').map(read_line).collect::<Result<_, _>>()?;

        Ok(Day8 {
            state: SolveState::new(),
            readouts,
        })
    }
}

fn read_line(line: &str) -> Result<String, BadInputError> {
    let (patterns, readout) = line
        .split('|')
        .map(read_patterns)
        .collect_tuple()
        .ok_or(BadInputError(line.to_string()))?;
    readout
        .into_iter()
        .map(|pattern| read_bad_pattern(patterns, pattern))
        .collect::<Result<_, _>>()
}

fn read_patterns(patts: &str) -> Vec<Segments> {
    patts.split_whitespace().map(read_segments).collect()
}

fn read_segments(patt: &str) -> Segments {
    patt.chars().map(bit_segment_from).sum()
}

fn bit_segment_from(seg: char) -> Segments {
    // a -> 0001
    // b -> 0010
    // c -> 0100
    // d -> 1000
    1 << (seg as usize) - ('a' as usize)
}

fn read_bad_pattern(
    patterns: Vec<Segments>,
    pattern: Segments,
) -> Result<&'static str, BadInputError> {
    recognize_segment_count(pattern)
        .or_else(|| recognize_segment_differences(patterns, pattern))
        .ok_or(BadInputError(pattern.to_string()))
}

fn recognize_segment_count(pattern: Segments) -> Option<&'static str> {
    match pattern.count_ones() {
        2 => Some("1"),
        3 => Some("7"),
        4 => Some("4"),
        7 => Some("8"),
        _ => None,
    }
}

fn recognize_segment_differences(
    patterns: Vec<Segments>,
    pattern: Segments,
) -> Option<&'static str> {
    // The number of segments in a pattern are stable.
    // i.e. Patterns that share three lit segments on a normal display
    //      will share three lit segments on a messed-up display.
    // e.g. '7' and '1' ordinarily share two lit segments, so in
    //      the messed up patterns they will still share two lit segments.
    //
    // On it's own, that isn't enough to distinguish a pattern;
    // e.g. '3' shares four segments with '2' and a different four segments with '5',
    //
    // However, if you sum together the number of segments
    // a single pattern shares with all the other patterns,
    // that sum will be unique for that pattern.
    let patt_1 = pattern;
    match patterns.iter().map(|patt_2| patt_1 & patt_2).sum() {
        36 => Some("0"),
        15 => Some("1"),
        29 => Some("2"),
        34 => Some("3"),
        26 => Some("4"),
        32 => Some("5"),
        35 => Some("6"),
        22 => Some("7"),
        42 => Some("8"),
        39 => Some("9"),
        _ => None,
    }
}

impl Iterator for Day8 {
    type Item = Box<dyn Debug>;

    fn next(&mut self) -> Option<Self::Item> {
        self.state.next()
    }
}
