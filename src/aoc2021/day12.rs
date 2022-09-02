use std::{
    collections::{HashMap, HashSet},
    fs::read_to_string,
    hash::Hash,
};

use crate::types::Answer;
use anyhow::Result;

#[derive(PartialEq, Eq, Hash)]
enum Cave<'a> {
    Entrance,
    Exit,
    Big(&'a str),
    Small(&'a str),
}

impl<'a> Cave<'a> {
    fn is_big(&self) -> bool {
        match self {
            Cave::Big(_) => true,
            _ => false,
        }
    }
    fn is_exit(&self) -> bool {
        match self {
            Cave::Exit => true,
            _ => false,
        }
    }
}

impl<'a> From<&'a str> for Cave<'a> {
    fn from(s: &'a str) -> Self {
        match s {
            "start" => Cave::Entrance,
            "end" => Cave::Exit,
            s if s.chars().all(char::is_uppercase) => Cave::Big(s),
            s => Cave::Small(s),
        }
    }
}

fn next_caves<'a, 'b>(
    options: &'a HashSet<Cave<'b>>,
    visited: HashSet<Cave>,
) -> HashSet<&'a Cave<'b>> {
    return options
        .into_iter()
        .filter(|cave| !visited.contains(cave) || cave.is_big())
        .collect::<HashSet<_>>();
}

fn read_input<'a>(path: &str) -> Result<Vec<[Cave; 2]>> {
    read_to_string(path)?
        .split("\n")
        .map(parse_line)
        .collect::<Result<Vec<_>>>()
}

fn parse_line<'a>(line: &'a str) -> Result<[Cave; 2]> {
    line.split('-')
        .map(Cave::from)
        .collect::<Vec<_>>()
        .try_into()
}

pub fn solve(path: &str) -> Result<(Answer, Answer)> {
    let connections = read_input(path)?;

    let map = HashMap::<Cave, HashSet<Cave>>::new();
    for [a, b] in connections.iter() {
        map.entry(a).or_insert_with(HashSet::new).push(b);
        map.entry(b).or_insert_with(HashSet::new).push(a);
    }
    let visited = HashSet::new();

    let tunnels = Vec::<HashSet<Cave>>::new();

    let asdf = next_caves(connections.get(Cave::Entrance), &visited);
    for x in asdf {
        let qwerty = next_caves(connections.get(Cave::Entrance), &visited);
    }

    return Ok((Ok(5), Ok(5)));
}
