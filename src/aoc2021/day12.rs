use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
};

use crate::types::{Answer, BadInputError};
use anyhow::Result;

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
enum Cave {
    Entrance,
    Exit,
    Big(String),
    Small(String),
}

impl Cave {
    fn is_big(&self) -> bool {
        matches!(self, Cave::Big(_))
    }

    fn is_exit(&self) -> bool {
        matches!(self, Cave::Exit)
    }
}

impl From<&str> for Cave {
    fn from(s: &str) -> Self {
        match s {
            "start" => Cave::Entrance,
            "end" => Cave::Exit,
            s if s.chars().all(char::is_uppercase) => Cave::Big(s.to_string()),
            s => Cave::Small(s.to_string()),
        }
    }
}

fn next_caves<'a, 'b>(options: &'a HashSet<Cave>, visited: HashSet<Cave>) -> HashSet<&'a Cave> {
    return options
        .into_iter()
        .filter(|cave| !visited.contains(cave) || cave.is_big())
        .collect::<HashSet<_>>();
}

fn read_input(input: &str) -> Result<Vec<[Cave; 2]>, Vec<Cave>> {
    input.split("\n").map(parse_line).collect()
}

fn parse_line(line: &str) -> Result<[Cave; 2], Vec<Cave>> {
    line.split('-')
        .map(Cave::from)
        .collect::<Vec<_>>()
        .try_into()
}

pub fn solve(input: &str) -> Result<(Answer, Answer)> {
    let connections = read_input(input).map_err(|err| BadInputError(format!("{err:?}")))?;

    let mut map = HashMap::<Cave, HashSet<Cave>>::new();
    // for [a, b] in connections.iter() {
    //     map.entry(*a).or_insert_with(HashSet::new).insert(*b);
    //     map.entry(*b).or_insert_with(HashSet::new).insert(*a);
    // }
    // let visited = HashSet::new();

    let tunnels = Vec::<HashSet<Cave>>::new();

    // let asdf = next_caves(connections.get(Cave::Entrance), &visited);
    // for x in asdf {
    //     let qwerty = next_caves(connections.get(Cave::Entrance), &visited);
    // }

    return Ok((Ok(5), Ok(5)));
}
