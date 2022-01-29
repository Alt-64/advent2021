use std::{
    collections::{HashMap, HashSet},
    fs::read_to_string,
    hash::Hash,
};

use crate::types::{Error, Solution};

type Pattern<const S: usize> = [char; S];
type Rewiring<const S: usize> = HashMap<Pattern<S>, Pattern<S>>;

// Values are index-mapped, e.g. segment pattern for 1 is CORRECT_PATTERNS[1]
const CORRECT_PATTERNS: [Pattern<7>; 10] = [
    ['a', 'b', 'c', 'e', 'f', 'g', '\0'],
    ['c', 'f', '\0', '\0', '\0', '\0', '\0'],
    ['a', 'c', 'd', 'e', 'g', '\0', '\0'],
    ['a', 'c', 'd', 'f', 'g', '\0', '\0'],
    ['b', 'c', 'd', 'f', '\0', '\0', '\0'],
    ['a', 'b', 'd', 'f', 'g', '\0', '\0'],
    ['a', 'b', 'd', 'e', 'f', 'g', '\0'],
    ['a', 'c', 'f', '\0', '\0', '\0', '\0'],
    ['a', 'b', 'c', 'd', 'e', 'f', 'g'],
    ['a', 'b', 'c', 'd', 'f', 'g', '\0'],
];

fn read_seven_segment(pattern: &Pattern<7>) -> Option<char> {
    let val = match pattern.len() {
        2 => '1',
        3 => '7',
        4 => '4',
        5 if pattern.contains(&'b') => '5',
        5 if pattern.contains(&'e') => '2',
        5 => '3',
        6 if !pattern.contains(&'d') => '0',
        6 if pattern.contains(&'c') => '9',
        6 => '6',
        7 => '7',
        _ => return None,
    };
    Some(val)
}

pub fn solve(path: &str) -> Result<(Solution, Solution), Error> {
    let input = read_to_string(path)?;
    let entries: Vec<Entry<7, 10, 4>> = input
        .split('\n')
        .filter(|&c| c != "")
        .map(str::trim)
        .map(read_line)
        .collect::<Result<_, Error>>()?;

    let mut soln1: i64 = 0;
    let mut soln2: i64 = 0;
    for entry in entries {
        let digits = entry.interpret(&CORRECT_PATTERNS, read_seven_segment)?;
        soln1 = part1(soln1, &digits);
        soln2 = part2(soln2, &digits);
    }

    Ok((Ok(soln1), Ok(soln2)))
}

fn part1(mut soln1: i64, digits: &str) -> i64 {
    for digit in digits.chars() {
        match digit {
            '1' | '4' | '7' | '8' => soln1 += 1,
            _ => (),
        }
    }
    soln1
}

fn part2(soln2: i64, digits: &str) -> i64 {
    // digits *will* be parsable, since it was built internally.
    soln2 + digits.parse::<i64>().unwrap()
}

struct Entry<const S: usize, const P: usize, const O: usize> {
    patterns: [[char; S]; P],
    outputs: [[char; S]; O],
}

impl<const S: usize, const P: usize, const O: usize> Entry<S, P, O> {
    fn interpret(
        &self,
        cor_pats: &[Pattern<S>; P],
        read: fn(&Pattern<S>) -> Option<char>,
    ) -> Result<String, Error> {
        let display = BrokenSegmentDisplay::new(&self.patterns, cor_pats);

        for p in display.wiring.patterns {
            println!("{:?}", p);
        }
        for s in display.wiring.segments {
            println!("{:?}", s);
        }
        // let rewiring = display.determine_rewiring()?;

        // let mut digits = Vec::<char>::new();
        // for output in self.outputs {
        //     let intended_pattern = rewiring.get(&output).unwrap();
        //     let digit = read(intended_pattern).unwrap();
        //     digits.push(digit);
        // }
        // Ok(String::from_iter(digits))
        Ok("1111".to_string())
    }
}

struct PatternSet<'b, const P: usize, const S: usize> {
    incor: &'b [Pattern<S>; P],
    cor: &'b [Pattern<S>; P],
}
impl<'b, const P: usize, const S: usize> PatternSet<'b, P, S> {
    fn get_same_length_patterns(
        &self,
        length_matches: Box<dyn Fn(usize) -> bool>,
    ) -> HashSet<&'b Pattern<S>> {
        self.cor
            .iter()
            .filter(|&pat| length_matches(pat.get_segments().len()))
            .collect()
    }
}

struct Wiring<'b, const S: usize> {
    segments: HashMap<char, HashSet<char>>,
    patterns: HashMap<&'b Pattern<S>, HashSet<&'b Pattern<S>>>,
}
impl<'b, const P: usize, const S: usize> From<&PatternSet<'b, P, S>> for Wiring<'b, S> {
    fn from(patterns: &PatternSet<'b, P, S>) -> Self {
        Wiring {
            segments: SegmentWiring::get_poss_wirings(patterns),
            patterns: PatternWiring::get_poss_wirings(patterns),
        }
    }
}

struct BrokenSegmentDisplay<'b, const P: usize, const S: usize> {
    patterns: PatternSet<'b, P, S>,
    wiring: Wiring<'b, S>,
}
impl<'a, 'b: 'a, const P: usize, const S: usize> BrokenSegmentDisplay<'a, P, S> {
    fn new(incor: &'b [Pattern<S>; P], cor: &'b [Pattern<S>; P]) -> Self {
        let patterns = PatternSet { incor, cor };
        let wiring = Wiring::from(&patterns);
        BrokenSegmentDisplay { patterns, wiring }
    }
    fn determine_rewiring(&self) -> Result<Rewiring<S>, Error> {
        let rewiring = HashMap::new();
        Ok(rewiring)
    }
    // fn is_rewiring_determined(&self) -> {

    // }
}

trait Rewirable<'a, 'b: 'a, const P: usize, const S: usize> {
    type Item: Eq + Hash + Copy;

    fn new(patterns: &'a PatternSet<'b, P, S>) -> Self;
    fn get_items(&self) -> HashSet<Self::Item>;
    fn get_wiring_options_for(&self, item: Self::Item) -> HashSet<Self::Item>;

    fn get_poss_wirings(
        patterns: &'a PatternSet<'b, P, S>,
    ) -> HashMap<Self::Item, HashSet<Self::Item>>
    where
        Self: Sized,
    {
        let context = Self::new(patterns);
        let items = context.get_items();
        let mut mappings = HashMap::with_capacity(items.len());
        for item in items {
            let mapping_options = context.get_wiring_options_for(item);
            mappings.insert(item, mapping_options);
        }
        mappings
    }
}

struct SegmentWiring<'a, 'b: 'a, const P: usize, const S: usize> {
    patterns: &'a PatternSet<'b, P, S>,
}
impl<const P: usize, const S: usize> SegmentWiring<'_, '_, P, S> {
    fn get_wire_pattern_lengths(&self, segment: char) -> HashSet<usize> {
        self.patterns
            .incor
            .iter()
            .filter(|incor_pat| incor_pat.contains(&segment))
            .map(|incor_pat| incor_pat.get_segments().len())
            .collect()
    }
}
impl<'a, 'b: 'a, const P: usize, const S: usize> Rewirable<'a, 'b, P, S>
    for SegmentWiring<'a, 'b, P, S>
{
    type Item = char;

    fn new(patterns: &'a PatternSet<'b, P, S>) -> Self {
        Self { patterns }
    }

    fn get_items(&self) -> HashSet<Self::Item> {
        HashSet::from_iter(
            self.patterns
                .incor
                .iter()
                .map(|pattern| pattern.get_segments())
                .flatten()
                .map(|&x| x),
        )
    }

    fn get_wiring_options_for(&self, item: Self::Item) -> HashSet<Self::Item> {
        let lengths = self.get_wire_pattern_lengths(item);
        let length_matches = Box::new(move |len: usize| lengths.contains(&len));
        self.patterns
            .get_same_length_patterns(length_matches)
            .into_iter()
            .map(|pattern| pattern.get_segments())
            .flatten()
            .map(|&x| x)
            .collect()
    }
}

struct PatternWiring<'a, 'b, const P: usize, const S: usize> {
    patterns: &'a PatternSet<'b, P, S>,
}

impl<'a, 'b: 'a, const P: usize, const S: usize> Rewirable<'a, 'b, P, S>
    for PatternWiring<'a, 'b, P, S>
{
    type Item = &'b Pattern<S>;

    fn new(patterns: &'a PatternSet<'b, P, S>) -> Self {
        Self { patterns }
    }

    fn get_items(&self) -> HashSet<Self::Item> {
        HashSet::from_iter(self.patterns.incor.into_iter())
    }

    fn get_wiring_options_for(&self, item: Self::Item) -> HashSet<Self::Item> {
        let length = item.get_segments().len();
        let length_matches = Box::new(move |len: usize| len == length);
        self.patterns.get_same_length_patterns(length_matches)
    }
}

fn read_line<'a, const S: usize, const P: usize, const O: usize>(
    line: &'a str,
) -> Result<Entry<S, P, O>, Error> {
    let mut elts = line.split('|');
    let mut get_values = || elts.next().ok_or(Error::BadInput(line.to_owned()));

    let patterns: [[char; S]; P] = read_words(get_values()?)?;
    let outputs: [[char; S]; O] = read_words(get_values()?)?;
    Ok(Entry { patterns, outputs })
}

fn read_words<const S: usize, const P: usize>(s: &str) -> Result<[[char; S]; P], Error> {
    s.split_whitespace()
        .map(read_segments)
        .collect::<Result<Vec<[char; S]>, Error>>()?
        .try_into()
        .map_err(Into::into)
}

fn read_segments<const S: usize>(word: &str) -> Result<[char; S], Error> {
    let chars = word.chars().collect::<Vec<char>>();

    if chars.len() > S {
        return Err(Error::BadInput(word.to_string()));
    }

    let mut segments = ['\0'; S];
    for (i, &c) in chars.iter().enumerate() {
        segments[i] = c;
    }
    Ok(segments)
}

trait SegmentPattern {
    fn get_segments<'a>(&'a self) -> &'a [char];
}

impl<const S: usize> SegmentPattern for Pattern<S> {
    fn get_segments<'a>(&'a self) -> &'a [char] {
        let length = self
            .iter()
            .enumerate()
            .find_map(|(i, &segment)| if segment == '\0' { Some(i) } else { None })
            .unwrap_or(self.len());
        &self[0..length]
    }
}
