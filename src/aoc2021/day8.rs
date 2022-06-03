use std::{collections::HashMap, fs::read_to_string};

use crate::types::{ Solution, BadInputError, NoSolutionError};
use anyhow::Result;

// Patterns are read in as strings of characters, but the information content
// (which segments are lit) can be compressed down to a bitfield stored
// as an integer.  The bitfield form reduces the memory footprint of the
// patterns and makes comparing patterns a little easier.
//
// To match a pattern to a miswired pattern, we first determine how patterns
// are unique within their set.  If two patterns within a set differ by a single
// segment being lit, their matches in the miswired set will also differ
// by a single segment, though probably a different segment.  If you take this
// thought to it's logical extreme and count the number of different segments
// between a single pattern and all the others in it's set, you'll get a
// representation of that pattern that will match against at
// least one other pattern in the opposite set.
//
// If there is just one match, the job is done.  You can even infer some of the
// other pattern pairings based on this 'diff-set' if you'd like.
//
// If there is more than one match, we have to use one of the patterns that
// did have a one-to-one match to infer a pairing.  If none exists, it is
// impossible to determine a map from the wired set to the miswired set.

type Segment = usize;
type Pattern = usize;

type Entry<const P: usize, const O: usize> = ([Pattern; P], [Pattern; O]);

// Values are index-mapped, e.g. segment pattern for 7 is CORRECT_PATTERNS[7]
const CORRECT_PATTERNS: [Pattern; 10] = [
    0b1110111, 0b0100100, 0b1011101, 0b1101101, 0b0101110, 0b1101011, 0b1111011, 0b0100101,
    0b1111111, 0b1101111,
];

fn read_seven_segment(pattern: Pattern) -> Result<char> {
    Ok(match pattern {
        0b1110111 => '0',
        0b0100100 => '1',
        0b1011101 => '2',
        0b1101101 => '3',
        0b0101110 => '4',
        0b1101011 => '5',
        0b1111011 => '6',
        0b0100101 => '7',
        0b1111111 => '8',
        0b1101111 => '9',
        _ => return Err(BadInputError(pattern.to_string())),
    })
}

// a -> 1, b -> 2, c -> 4, d -> 8
fn bit_segment_from(char_segment: char) -> Segment {
    let x = (char_segment as usize) - ('a' as usize);
    1 << x
}

pub fn solve(path: &str) -> Result<(Solution, Solution)> {
    let input = read_to_string(path)?;
    let entries: Vec<Entry<10, 4>> = input
        .split('\n')
        .filter(|&c| c != "")
        .map(str::trim)
        .map(read_line)
        .collect::<Result<_>>()?;

    let cor_hashes = CORRECT_PATTERNS.map(|pattern| {
        let diffs = get_diffs_for(pattern, CORRECT_PATTERNS);
        calc_diff_hash(diffs)
    });
    let cor_pat_hashmap = HashMap::from(cor_hashes.zip(CORRECT_PATTERNS));

    let mut soln1: i64 = 0;
    let mut soln2: i64 = 0;

    for entry in entries {
        let (incor_patterns, output_patterns) = entry;
        let inferences = output_patterns.map(|pattern| {
            first_round_inference::<10, 4>(pattern, incor_patterns, &cor_pat_hashmap)
                .map(|inferred_pattern| (pattern, inferred_pattern))
        });

        let inferred_pairs = inferences.iter().flatten().cloned().collect::<Vec<_>>();

        let digits = inferences
            .iter()
            .enumerate()
            .map(|(i, pair)| {
                pair.or_else(|| {
                    second_round_inference(i, &inferred_pairs, CORRECT_PATTERNS, incor_patterns)
                })
                .ok_or(NoSolutionError)
                .map(|(_incorrect, inferred)| read_seven_segment(inferred))?
            })
            .collect::<Result<String, _>>()?;

        soln1 += part1(&digits);
        soln2 += part2(&digits);
    }

    Ok((Ok(soln1), Ok(soln2)))
}

fn first_round_inference<const P: usize, const O: usize>(
    pattern: Pattern,
    incor_patterns: [Pattern; P],
    cor_pat_hashmap: &HashMap<u32, usize>,
) -> Option<Pattern> {
    let diffs = get_diffs_for(pattern, incor_patterns);
    let hash = calc_diff_hash(diffs);
    cor_pat_hashmap.get(&hash).cloned()
}

fn second_round_inference<const P: usize>(
    i: usize,
    inferred_pairs: &[(Pattern, Pattern)],
    cor_patterns: [Pattern; P],
    incor_patterns: [Pattern; P],
) -> Option<(Pattern, Pattern)> {
    for (incor_pattern, inferred_pattern) in inferred_pairs {
        let incor_diffs = get_diffs_for(*incor_pattern, incor_patterns);
        let cor_diffs = get_diffs_for(*inferred_pattern, cor_patterns);
        let matching_indices = cor_diffs
            .into_iter()
            .enumerate()
            .filter(|(_, d)| *d == incor_diffs[i])
            .map(|(j, _)| j)
            .collect::<Vec<_>>();
        if matching_indices.len() == 1 {
            let j = matching_indices[0];
            return Some((*incor_pattern, CORRECT_PATTERNS[j]));
        }
    }
    None
}

fn part1(digits: &str) -> i64 {
    let mut acc = 0;
    for digit in digits.chars() {
        match digit {
            '1' | '4' | '7' | '8' => acc += 1,
            _ => (),
        }
    }
    acc
}

fn part2(digits: &str) -> i64 {
    // digits *will* be parsable, since it was built internally.
    digits.parse::<i64>().unwrap()
}

fn read_line<'a, const P: usize, const O: usize>(line: &'a str) -> Result<Entry<P, O>> {
    let mut elts = line.split('|');
    let mut get_values = || elts.next().ok_or(BadInputError(line.to_owned()));

    let patterns = read_words(get_values()?)?;
    let outputs = read_words(get_values()?)?;
    Ok((patterns, outputs))
}

fn read_words<const P: usize>(s: &str) -> Result<[usize; P]> {
    s.split_whitespace()
        .map(read_segments)
        .collect::<Vec<usize>>()
        .try_into()
}

fn read_segments(word: &str) -> usize {
    word.chars().map(bit_segment_from).sum()
}

fn get_diffs_for<const P: usize>(pattern: Pattern, patterns: [Pattern; P]) -> [u32; P] {
    let p1 = pattern;
    patterns.map(|p2| p1 ^ p2).map(usize::count_ones)
}

fn calc_diff_hash<const P: usize>(diffs: [u32; P]) -> u32 {
    diffs.into_iter().map(hash).sum::<u32>()
}

fn hash(mut x: u32) -> u32 {
    x ^= x >> 16;
    x = x.overflowing_mul(0x7feb352d).0;
    x ^= x >> 15;
    x = x.overflowing_mul(0x846ca68b).0;
    x ^= x >> 16;
    return x;
}
