use crate::types::{BadInputError, Solution};
use anyhow::Result;
use itertools::Itertools;
use std::sync::mpsc::Sender;

type Segments = usize;

fn solve(input: &str, tx: Sender<(usize, usize, Solution)>) -> anyhow::Result<()> {
    let readouts: Vec<_> = input.split('\n').map(read_line).collect::<Result<_, _>>()?;
    let soln_1 = readouts
        .join("")
        .chars()
        .filter(|digit| ['1', '4', '7', '8'].contains(digit))
        .count() as u16;
    tx.send((8, 1, Ok(Box::new(soln_1))));
    let soln_2: u16 = readouts.iter().map(|x| x.parse::<u16>().unwrap()).sum();
    tx.send((8, 2, Ok(Box::new(soln_1))));

    Ok(())
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
