use std::collections::HashMap;

use crate::types::{Answer, BadInputError};
use anyhow::Result;
use itertools::Itertools;

type Segments = usize;
pub fn solve(input: &str) -> Result<(Answer, Answer)> {
    let mut soln1: i64 = 0;
    let mut soln2: i64 = 0;

    // The number of segments in a pattern are stable.
    // i.e. Patterns that share three lit segments on a normal display
    //      will share three lit segments on a messed-up display.
    // e.g. 7 and 1 ordinarily share two lit segments, so in
    //      the messed up patterns they will still share two lit segments.
    //
    // On it's own, that isn't enough to distinguish a pattern.
    // However, if you sum together the number of segments
    // a single pattern shares with all the other patterns,
    // that sum will be unique for that pattern.
    let segment_relations = HashMap::<usize, &str>::from([
        (36, "0"),
        (15, "1"),
        (29, "2"),
        (34, "3"),
        (26, "4"),
        (32, "5"),
        (35, "6"),
        (22, "7"),
        (42, "8"),
        (39, "9"),
    ]);

    for (all_patts, output_patts) in read_input(input) {
        let recognizer = Recognizer::new(all_patts, &segment_relations);
        let digits = output_patts
            .into_iter()
            .map(|pattern| recognizer.read_bad_pattern(pattern))
            .collect::<Result<String, BadInputError>>()?;

        soln1 += part1(&digits);
        soln2 += part2(&digits);
    }

    Ok((Box::new(soln1), Box::new(soln2)))
}

struct Recognizer<'a> {
    patterns: Vec<Segments>,
    segment_relations: &'a HashMap<usize, &'static str>,
}

impl<'a> Recognizer<'a> {
    fn new(patterns: Vec<usize>, segment_relations: &'a HashMap<usize, &'static str>) -> Self {
        Recognizer {
            patterns,
            segment_relations,
        }
    }

    fn read_bad_pattern(&self, pattern: Segments) -> Result<&'static str, BadInputError> {
        Recognizer::recognize_segment_count(pattern)
            .or_else(|| self.recognize_relations(pattern))
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

    fn recognize_relations(&self, patt_1: Segments) -> Option<&'static str> {
        let shares: usize = self.patterns.iter().map(|patt_2| patt_1 & patt_2).sum();
        return self.segment_relations.get(&shares).map(|&x| x);
    }
}

fn part1(digits: &str) -> i64 {
    digits
        .chars()
        .filter(|digit| ['1', '4', '7', '8'].contains(digit))
        .count() as i64
}

fn part2(digits: &str) -> i64 {
    digits.parse::<i64>().unwrap()
}

fn read_input(input: &str) -> Vec<(Vec<Segments>, Vec<Segments>)> {
    input.split('\n').map(read_line).collect()
}

fn read_line(line: &str) -> (Vec<Segments>, Vec<Segments>) {
    line.split('|').map(read_patterns).collect_tuple().unwrap()
}

fn read_patterns(patts_str: &str) -> Vec<Segments> {
    patts_str.split_whitespace().map(read_segments).collect()
}

fn read_segments(patt_str: &str) -> usize {
    patt_str.chars().map(bit_segment_from).sum()
}

// a -> 1, b -> 2, c -> 4, d -> 8
fn bit_segment_from(char_segment: char) -> Segments {
    let x = (char_segment as usize) - ('a' as usize);
    1 << x
}
