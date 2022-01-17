use std::{
    collections::{HashMap, HashSet},
    fs::read_to_string,
    hash::Hash,
};

use crate::types::{Error, Solution};

#[derive(Debug)]
enum Binding {
    Known(char),
    Unknown(HashSet<char>),
}

const PATTERN_1: [char; 2] = ['c', 'f'];
const PATTERN_2: [char; 5] = ['a', 'c', 'd', 'e', 'g'];
const PATTERN_3: [char; 5] = ['a', 'c', 'd', 'f', 'g'];
const PATTERN_4: [char; 4] = ['b', 'c', 'd', 'f'];
const PATTERN_5: [char; 5] = ['a', 'b', 'd', 'f', 'g'];
const PATTERN_6: [char; 6] = ['a', 'b', 'd', 'e', 'f', 'g'];
const PATTERN_7: [char; 3] = ['a', 'c', 'f'];
const PATTERN_8: [char; 7] = ['a', 'b', 'c', 'd', 'e', 'f', 'g'];
const PATTERN_9: [char; 6] = ['a', 'b', 'c', 'd', 'f', 'g'];
const PATTERN_0: [char; 6] = ['a', 'b', 'c', 'e', 'f', 'g'];

fn categorize_by_segment_count<'a, const P: usize>(
    patterns: &[&'a Pattern; P],
) -> Vec<Vec<&'a Pattern>> {
    // let categories = HashMap::<usize, Vec<&[char]>>::from([
    //     (2, Vec::with_capacity(1)),
    //     (3, Vec::with_capacity(1)),
    //     (4, Vec::with_capacity(1)),
    //     (5, Vec::with_capacity(3)),
    //     (6, Vec::with_capacity(3)),
    //     (7, Vec::with_capacity(1)),
    // ]);
    let mut categories = HashMap::<usize, Vec<&Pattern>>::new();
    for segments in patterns {
        let seg_count = segments.len();
        if let Some(category) = categories.get_mut(&seg_count) {
            category.push(segments)
        } else {
            categories.insert(seg_count, vec![segments]);
        }
    }
    let mut categories: Vec<_> = categories.into_values().collect();
    categories.sort_by(|a, b| a.len().cmp(&b.len()));
    categories.try_into().unwrap()
}

type PatternMap<'a, 'b, T> = HashMap<&'a T, &'b T>;

// fn determine<'a, 'b, T, const S: usize>(
//     mappings: PatternMap<T>,
//     incors: &[&'a T],
//     cors: &[&'b T],
// ) -> PatternMap<'a, 'b, T> {
//     let diffs = Vec::<char>::new();
//     for (i, a) in incors.iter().enumerate() {
//         diffs.extend(incors[i..].iter().map(|b| matching_wires(a, b)));
//     }

//     HashMap::new()
// }

// fn diff_wires(a: &[char], b: &[char]) -> Vec<char> {
//     a.iter()
//         .filter(|wire| !b.contains(wire))
//         .map(char::clone)
//         .collect()
// }

fn map_incorrect_to_correct<'a, 'b, const P: usize>(
    incor_patterns: &[&'a Pattern; P],
    cor_patterns: &[&'b Pattern; P],
) -> Result<HashMap<&'a Pattern, &'b Pattern>, Error> {
    if incor_patterns.len() != cor_patterns.len() {
        return Err(Error::BadInput(format!(
            "cannot map {} patterns to {} patterns",
            incor_patterns.len(),
            cor_patterns.len()
        )));
    }
    let incor_patterns = categorize_by_segment_count(incor_patterns);
    let cor_patterns = categorize_by_segment_count(cor_patterns);

    incor_patterns.iter().enumerate().for_each(|(i, x)| {
        let y: Vec<String> = x.iter().map(|y| y.iter().collect::<String>()).collect();
        println!("{} - {:?}", i, y);
    });

    let mut mappings = HashMap::with_capacity(incor_patterns.len());
    // for (incors, cors) in incor_patterns.into_iter().zip(cor_patterns) {
    //     if incors.len() != cors.len() {
    //         return Err(Error::NoSolution);
    //     }
    //     if incors.len() == 1 {
    //         mappings.insert(incors[0], cors[0]);
    //     } else {
    //         // mappings.extend(determine(mappings, &incors, &cors))
    //     }
    // }

    Ok(mappings)
}

type Pattern = [char];

struct Entry<const P: usize, const O: usize> {
    patterns: [Vec<char>; P],
    outputs: [Vec<char>; O],
}

pub fn solver(path: &str) -> Result<(Solution, Solution), Error> {
    let input = read_to_string(path)?;
    let entries: Vec<Entry<10, 4>> = input
        .split('\n')
        .filter(|&c| c != "")
        .map(str::trim)
        .map(read_line)
        .collect::<Result<_, Error>>()?;

    let cor_patterns: [&[char]; 10] = [
        &PATTERN_0, &PATTERN_1, &PATTERN_2, &PATTERN_3, &PATTERN_4, &PATTERN_5, &PATTERN_6,
        &PATTERN_7, &PATTERN_8, &PATTERN_9,
    ];

    // let value_map = HashMap::from(cor_patterns.iter().enumerate().collect());

    let mut soln1: Solution = Ok(0);
    let mut soln2: Solution = Ok(0);
    for Entry { outputs, patterns } in entries {
        let patterns: Vec<&[char]> = patterns.iter().map(Vec::as_slice).collect();
        let mappings = map_incorrect_to_correct(&patterns.try_into()?, &cor_patterns)?;
        // let digits: String = outputs
        //     .into_iter()
        //     .map(|o| value_map.get(mappings.get(&o).unwrap()).unwrap())
        //     .collect::<String>();
        // part1 never errors, part2 can error
        // soln1 = soln1.map(|soln| part1(soln, &digits));
        // soln2 = soln2.and_then(|soln| part2(soln, &digits));
    }

    Ok((soln1, soln2))
}

fn part1(soln1: i64, digits: &String) -> i64 {
    digits.chars().fold(soln1, |acc, digit| match digit {
        '1' | '4' | '7' | '8' => acc + 1,
        _ => acc,
    })
}

fn part2(soln2: i64, digits: &String) -> Result<i64, Error> {
    let value = digits.parse::<i64>()?;
    Ok(soln2 + value)
}

fn read_line<'a, const P: usize, const O: usize>(line: &'a str) -> Result<Entry<P, O>, Error> {
    let mut elts = line.split('|').map(read_values);
    let mut get_values = || elts.next().ok_or(Error::BadInput(line.to_owned()));

    let patterns: [Vec<char>; P] = get_values()?.try_into()?;
    let outputs: [Vec<char>; O] = get_values()?.try_into()?;
    Ok(Entry { patterns, outputs })
}

fn read_values(s: &str) -> Vec<Vec<char>> {
    s.split_whitespace()
        .map(str::chars)
        .map(Iterator::collect)
        .collect()
}
