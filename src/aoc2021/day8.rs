use std::{
    collections::{HashMap, HashSet},
    fs::read_to_string,
    hash::Hash,
};

use crate::types::{Error, Solution};

type Pattern<const S: usize> = [char; S];
type PatternSet<const S: usize, const P: usize> = [Pattern<S>; P];

type SegmentWiring = HashMap<char, HashSet<char>>;
type PatternWiring<'b, const S: usize> = HashMap<&'b Pattern<S>, HashSet<&'b Pattern<S>>>;

type Entry<const S: usize, const P: usize, const O: usize> = (PatternSet<S, P>, PatternSet<S, O>);
type Rewiring<const S: usize> = HashMap<Pattern<S>, Pattern<S>>;

// Values are index-mapped, e.g. segment pattern for 7 is CORRECT_PATTERNS[7]
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

fn read_seven_segment(pattern: Pattern<7>) -> Result<char, Error> {
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
        _ => return Err(Error::Unrecognized(pattern.iter().cloned().collect())),
    };
    Ok(val)
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
        let digits = interpret(entry, &CORRECT_PATTERNS)?
            .into_iter()
            .map(read_seven_segment)
            .collect::<Result<String, _>>()?;

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

fn interpret<const S: usize, const P: usize, const O: usize>(
    entry: Entry<S, P, O>,
    cor_set: &PatternSet<S, P>,
) -> Result<PatternSet<S, O>, Error> {
    let (incor_set, output) = entry;
    let rewiring = determine_rewiring(incor_set, cor_set)?;
    let intended_output = output.map(|o| *rewiring.get(&o).unwrap());
    Ok(intended_output)
}

fn determine_rewiring<'b, const S: usize, const P: usize>(
    incor_set: PatternSet<S, P>,
    cor_set: &'b PatternSet<S, P>,
) -> Result<Rewiring<S>, Error> {
    let mut seg_wiring = SegmentWiring::new_wiring(&cor_set, &incor_set);
    let mut pat_wiring = PatternWiring::new_wiring(&cor_set, &incor_set);

    let mut changed: bool;
    loop {
        Rewirable::<_, _, S>::print(&seg_wiring);
        pat_wiring.print();
        if let Some(res) = rewire(&seg_wiring, &pat_wiring) {
            return res;
        }
        changed = seg_wiring.trim(&pat_wiring) || pat_wiring.trim(&seg_wiring);
        if !changed {
            // No more changes could be inferred, therefore it is impossible to
            // determine the correct wiring
            return Err(Error::NoSolution);
        }
    }
}

fn rewire<const S: usize>(
    seg_wiring: &SegmentWiring,
    pat_wiring: &PatternWiring<S>,
) -> Option<Result<Rewiring<S>, Error>> {
    None
}

trait Rewirable<'a, T, C, const S: usize>
where
    Self: Sized + FromIterator<(T, HashSet<T>)>,
    T: Eq + Hash + Copy,
    C: Eq + Hash + Copy,
{
    fn print(&self);

    fn get_nodes<const P: usize>(incor_set: &'a PatternSet<S, P>) -> HashSet<T>;
    fn get_options_for<const P: usize>(node: T, cor_set: &'a PatternSet<S, P>) -> HashSet<T>;

    fn new_wiring<const P: usize>(
        cor_set: &'a PatternSet<S, P>,
        incor_set: &'a PatternSet<S, P>,
    ) -> Self {
        Self::get_nodes(incor_set)
            .into_iter()
            .map(|item| (item, Self::get_options_for(item, cor_set)))
            .collect()
    }

    fn connection_exists_for(node: &T, complement_node: &C) -> bool;

    fn iter_edges<'b>(&'b self) -> Box<dyn Iterator<Item = (&'b T, &'b HashSet<T>)> + 'b>;
    fn iter_mut_edges<'b>(
        &'b mut self,
    ) -> Box<dyn Iterator<Item = (&'b T, &'b mut HashSet<T>)> + 'b>;

    // Every incor_seg in all incor_pats must be pairable to a cor_seg in the
    // cor_pat.
    // Conversely, any incor_seg not in incor_pat cannot pair with any cor_segs in cor_pat.
    // This function combs through the graph in self and compares it with the
    // complement graph to trim wiring possibilies from self following the rules
    // described above.
    fn trim<Complement: Rewirable<'a, C, T, S>>(&mut self, complement_wiring: &Complement) -> bool {
        let mut changed = false;
        for (incor_comp, cor_comps) in complement_wiring.iter_edges() {
            if cor_comps.len() != 1 {
                continue;
            }
            let cor_comp = cor_comps.iter().next().unwrap();

            for (&incor_node, cor_nodes) in self.iter_mut_edges() {
                let untrimmed_len = cor_nodes.len();

                let connected = Self::connection_exists_for(&incor_node, &incor_comp);
                cor_nodes.drain_filter(|cor_node| {
                    connected != Complement::connection_exists_for(cor_comp, cor_node)
                });

                let trimmed_len = cor_nodes.len();
                changed = changed || untrimmed_len != trimmed_len;
            }
        }
        changed
    }
}

impl<'a, const S: usize> Rewirable<'a, char, &'a Pattern<S>, S> for SegmentWiring {
    fn print(&self) {
        for (incor_seg, cor_segs) in self {
            println!("{} -> {:?}", incor_seg, cor_segs);
        }
    }

    fn get_nodes<const P: usize>(incor_set: &'a PatternSet<S, P>) -> HashSet<char> {
        HashSet::from_iter(
            incor_set
                .iter()
                .map(|pattern| pattern.get_segments())
                .flatten()
                .cloned(),
        )
    }

    fn iter_edges<'b>(&'b self) -> Box<dyn Iterator<Item = (&'b char, &'b HashSet<char>)> + 'b> {
        Box::new(self.iter())
    }

    fn iter_mut_edges<'b>(
        &'b mut self,
    ) -> Box<dyn Iterator<Item = (&'b char, &'b mut HashSet<char>)> + 'b> {
        Box::new(self.iter_mut())
    }

    fn get_options_for<const P: usize>(
        segment: char,
        cor_set: &'a PatternSet<S, P>,
    ) -> HashSet<char> {
        let poss_lengths = get_segment_poss_lengths(cor_set, segment);
        cor_set
            .iter()
            .map(|pattern| pattern.get_segments())
            .filter(|segments| poss_lengths.contains(&segments.len()))
            .flatten()
            .cloned()
            .collect()
    }

    fn connection_exists_for(segment: &char, pattern: &&'a Pattern<S>) -> bool {
        pattern.contains(segment)
    }
}

impl<'a, const S: usize> Rewirable<'a, &'a Pattern<S>, char, S> for PatternWiring<'a, S> {
    fn print(&self) {
        for (incor_pat, cor_pats) in self {
            println!("{} -> ", String::from_iter(incor_pat.get_segments()));
            for cor_pat in cor_pats {
                println!("\t{}", String::from_iter(cor_pat.get_segments()));
            }
        }
    }

    fn get_nodes<const P: usize>(incor_set: &'a PatternSet<S, P>) -> HashSet<&'a Pattern<S>> {
        HashSet::from_iter(incor_set.into_iter())
    }

    fn iter_edges<'b>(
        &'b self,
    ) -> Box<dyn Iterator<Item = (&'b &'a Pattern<S>, &'b HashSet<&'a Pattern<S>>)> + 'b> {
        Box::new(self.iter())
    }

    fn iter_mut_edges<'b>(
        &'b mut self,
    ) -> Box<dyn Iterator<Item = (&'b &'a Pattern<S>, &'b mut HashSet<&'a Pattern<S>>)> + 'b> {
        Box::new(self.iter_mut())
    }

    fn get_options_for<const P: usize>(
        pattern: &'a Pattern<S>,
        cor_set: &'a PatternSet<S, P>,
    ) -> HashSet<&'a Pattern<S>> {
        let length = pattern.get_segments().len();
        cor_set
            .iter()
            .filter(|&pattern| pattern.get_segments().len() == length)
            .collect()
    }

    fn connection_exists_for(pattern: &&'a Pattern<S>, segment: &char) -> bool {
        pattern.contains(segment)
    }
}

fn get_segment_poss_lengths<const S: usize, const P: usize>(
    pattern_set: &PatternSet<S, P>,
    segment: char,
) -> HashSet<usize> {
    pattern_set
        .iter()
        .filter_map(|&pattern| {
            if pattern.contains(&segment) {
                Some(pattern.get_segments().len())
            } else {
                None
            }
        })
        .collect()
}

fn read_line<'a, const S: usize, const P: usize, const O: usize>(
    line: &'a str,
) -> Result<Entry<S, P, O>, Error> {
    let mut elts = line.split('|');
    let mut get_values = || elts.next().ok_or(Error::BadInput(line.to_owned()));

    let patterns: PatternSet<S, P> = read_words(get_values()?)?;
    let outputs: PatternSet<S, O> = read_words(get_values()?)?;
    Ok((patterns, outputs))
}

fn read_words<const S: usize, const P: usize>(s: &str) -> Result<PatternSet<S, P>, Error> {
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

    segments[0..chars.len()].sort();
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
