use std::{
    collections::HashMap,
    fs::read_to_string,
    hash::{BuildHasherDefault, Hasher},
};

use num::traits::ops::overflowing::OverflowingAdd;

use crate::types::{Error, Solution};

// Patterns are read in as strings of characters, but the information content
// (which segments are lit) can be compressed down to a bitfield stored
// as an integer.  The bitfield form reduces the memory footprint of the
//patterns and makes comparing patterns simpler.
//
// To determine which pattern matches with a miswired pattern, we
// examine how each pattern is unique within it's set.  If two patterns
// within a set differ by a single segment being lit, the matching patterns
// in the miswired set will also differ by a single segment, potentially
// a different segment.  By counting the number of different segments between a
// pattern and all the others in it's set, we have a representation of that
// pattern that will match against at least one other pattern in the opposite
// set.
//
// If there is just one match, the job is done, and we have a pair of matched
// patterns.
//
// If there is more than one match, we must try to use one of the patterns that
// did have a one-to-one match to identify a pair.  By carrying the ordering
// of the patterns in the set to the set of differences, we can recognize which
// pattern

type Segment = usize;
type Pattern = usize;

type Entry<const P: usize, const O: usize> = ([Pattern; P], [Pattern; O]);

// Values are index-mapped, e.g. segment pattern for 7 is CORRECT_PATTERNS[7]
const CORRECT_PATTERNS: [Pattern; 10] = [
    0b1110111, 0b0100100, 0b1011101, 0b1101101, 0b0101110, 0b1101011, 0b1111011, 0b0100101,
    0b1111111, 0b1101111,
];

fn read_seven_segment(pattern: Pattern) -> Result<char, Error> {
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
        _ => return Err(Error::Unrecognized(pattern.to_string())),
    })
}

// a -> 1, b -> 2, c -> 4, d -> 8
fn bit_segment_from(char_segment: char) -> Segment {
    let x = (char_segment as usize) - ('a' as usize);
    1 << x
}

pub fn solve(path: &str) -> Result<(Solution, Solution), Error> {
    let input = read_to_string(path)?;
    let entries: Vec<Entry<10, 4>> = input
        .split('\n')
        .filter(|&c| c != "")
        .map(str::trim)
        .map(read_line)
        .collect::<Result<_, Error>>()?;

    let wire_hash_builder = BuildHasherDefault::<WireHasher>::default();
    let cor_hashes = CORRECT_PATTERNS.map(|pattern| get_diffs_for(pattern, CORRECT_PATTERNS));
    let mut cor_pat_hashmap =
        HashMap::with_capacity_and_hasher(CORRECT_PATTERNS.len(), wire_hash_builder);
    for (k, v) in cor_hashes.zip(CORRECT_PATTERNS) {
        println!("{:?}", k);
        cor_pat_hashmap.insert(k, v);
    }

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
                .ok_or(Error::NoSolution)
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
    cor_pat_hashmap: &HashMap<[u32; P], usize, BuildHasherDefault<WireHasher>>,
) -> Option<Pattern> {
    let hash = get_diffs_for(pattern, incor_patterns);
    let asdf = cor_pat_hashmap.get(&hash).cloned();
    println!("{hash:?}\n{asdf:?}");
    asdf
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

fn read_line<'a, const P: usize, const O: usize>(line: &'a str) -> Result<Entry<P, O>, Error> {
    let mut elts = line.split('|');
    let mut get_values = || elts.next().ok_or(Error::BadInput(line.to_owned()));

    let patterns = read_words(get_values()?)?;
    let outputs = read_words(get_values()?)?;
    Ok((patterns, outputs))
}

fn read_words<const P: usize>(s: &str) -> Result<[usize; P], Error> {
    s.split_whitespace()
        .map(read_segments)
        .collect::<Option<Vec<usize>>>()
        .map(TryInto::try_into)
        .map(Result::ok)
        .flatten()
        .ok_or(Error::BadInput(s.to_string()))
}

fn read_segments(word: &str) -> Option<usize> {
    word.chars()
        .map(bit_segment_from)
        .reduce(|pattern, segment| pattern + segment)
}

fn calc_pattern_hash<const P: usize>(pattern: Pattern, patterns: [Pattern; P]) -> u32 {
    let diffs = get_diffs_for(pattern, patterns);
    let hash = calc_diff_hash(&diffs);
    return hash;
}

fn get_diffs_for<const P: usize>(pattern: Pattern, patterns: [Pattern; P]) -> [u32; P] {
    let p1 = pattern;
    patterns.map(|p2| p1 ^ p2).map(usize::count_ones)
}

fn calc_diff_hash(diffs: &[u32]) -> u32 {
    let mut sum = 0;
    for &x in diffs {
        sum = sum.overflowing_add(&hash(x)).0;
    }
    sum
}

#[derive(Default)]
struct WireHasher {
    state: u64,
}

impl Hasher for WireHasher {
    fn finish(&self) -> u64 {
        println!("{}", self.state);
        self.state
    }

    fn write(&mut self, bytes: &[u8]) {
        // We'll only be writing u32s, i.e. chunks of 4 u8s
        // print!("{:?}", u64::from(bytes);
        let (chunks, _) = bytes.as_chunks::<4>();
        for &chunk in chunks {
            let x = u32::from_be_bytes(chunk);
            let y = hash(x) as u64;
            // We don't want the order of the u32s to matter, i.e. we want
            // an association operation across the u32s.
            self.state = self.state.overflowing_add(y).0;
        }
    }
}

fn hash(mut x: u32) -> u32 {
    x ^= x >> 16;
    x = x.overflowing_mul(0x7feb352d).0;
    x ^= x >> 15;
    x = x.overflowing_mul(0x846ca68b).0;
    x ^= x >> 16;
    return x;
}
