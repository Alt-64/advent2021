use std::fs::read_to_string;

use itertools::Itertools;
use nom::{
    bytes::complete::{take_while, take_while1},
    error::ParseError,
    multi::separated_list0,
    sequence::delimited,
    AsChar, IResult, InputTakeAtPosition,
};

pub fn marked<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
{
    input.split_at_position_complete(|item| {
        let c = item.as_char();
        c == '?' || c == '#'
    })
}

fn read_springs(input: &str) -> IResult<&str, Vec<&str>> {
    delimited(
        take_while(|x| x == '.'),
        separated_list0(
            take_while1(|x| x == '.'),
            take_while1(|x| x == '?' || x == '#'),
        ),
        take_while(|x| x == '.'),
    )(input)
}

struct State {
    pos: usize,
}

fn count_possibilities(record: &str, blocks: &str) -> usize {
    let mut expression = blocks
        .split(',')
        .flat_map(|s| {
            let x: usize = s.parse().unwrap();
            Some(vec!['@'; x.saturating_sub(1)].iter().join(""))
        })
        .join("$!").chars().collect_vec();
    expression.insert(0, '!');
    expression.push('%');

    let len = expression.len();

    let mut heads_curr = vec![0; len];
    let mut heads_next = vec![0; len];
    heads_curr[0] = 1;

    for part in record.chars() {
        for i in 0..len {
            match (expression[i], part) {
                ('!', '.') => heads_next[i] += heads_curr[i],
                ('!', '#') => heads_next[i + 1] += heads_curr[i],
                ('!', _) => {
                    heads_next[i] += heads_curr[i];
                    heads_next[i + 1] += heads_curr[i];
                }

                ('@', '.') => (),
                ('@', _) => heads_next[i + 1] += heads_curr[i],

                ('$', '#') => (),
                ('$', _) => heads_next[i + 1] += heads_curr[i],

                ('%', '#') => (),
                ('%', _) => heads_next[i] += heads_curr[i],
                _ => panic!(),
            }
        }
        let tmp = heads_curr;
        heads_curr = heads_next;
        heads_next = tmp;
        for x in heads_next.iter_mut() {
            *x = 0;
        }
    }
    return *heads_curr.last().unwrap();
}

pub fn part1() -> usize {
    let input = read_to_string("input/12").unwrap();
    input
        .lines()
        .map(|line| {
            let mut parts = line.split_whitespace();
            let record = parts.next().unwrap();
            let blocks = parts.next().unwrap();
            count_possibilities(record, blocks)
        })
        .sum()
}

pub fn part2() -> usize {
    let input = read_to_string("input/12").unwrap();
    input
        .lines()
        .map(|line| {
            let mut parts = line.split_whitespace();
            let record = parts.next().unwrap();
            let record = vec![record; 5].join("?");
            let blocks = parts.next().unwrap();
            let blocks = vec![blocks; 5].join(",");
            count_possibilities(&record, &blocks)
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::count_possibilities;

    #[test]
    fn day12test1() {
        assert_eq!(4, count_possibilities("??????", "3"));
        assert_eq!(1, count_possibilities("???.###", "1,1,3"));
        assert_eq!(4, count_possibilities(".??..??...?##.", "1,1,3"));
        assert_eq!(1, count_possibilities("?#?#?#?#?#?#?#?", "1,3,1,6"));
        assert_eq!(1, count_possibilities("????.#...#...", "4,1,1"));
        assert_eq!(4, count_possibilities("????.######..#####.", "1,6,5"));
        assert_eq!(10, count_possibilities("?###????????", "3,2,1"));
    }
}
