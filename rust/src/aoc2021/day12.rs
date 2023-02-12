use std::{collections::HashSet, convert::TryInto, iter::from_fn};

use anyhow::Result;
use petgraph::{prelude::GraphMap, Undirected};

use crate::types::SolveState;
use std::fmt::Debug;

type Cave<'a> = &'a str;

struct Day12 {
    state: SolveState,
}

fn solve(value: &str) -> impl Iterator<Item = Box<dyn Debug>> {
    let mut graph;
    from_fn(|| {
        let mut i = 0;
        loop {
            i += 1;
            match i {
                0 => {}
            }
        }
    })
}

fn is_big(cave: Cave) -> bool {
    cave.len() == 2 && cave.chars().all(|c| c.is_uppercase())
}

fn is_small(cave: Cave) -> bool {
    cave.len() == 2 && cave.chars().all(|c| c.is_lowercase())
}

pub fn solve(input: &str) -> Result<(Answer, Answer)> {
    let connections = read_input(input);
    let graph = GraphMap::<Cave, HashSet<Cave>, Undirected>::from_edges(connections);

    let soln1 = find_paths(&graph, false).count() as i64;
    let soln2 = find_paths(&graph, true).count() as i64;

    return Ok((Box::new(soln1), Box::new(soln2)));
}

fn read_input(input: &str) -> Vec<(Cave, Cave)> {
    input.split("\n").map(parse_line).collect()
}

fn parse_line(line: &str) -> (Cave, Cave) {
    let [a, b]: [Cave; 2] = line
        .split('-')
        .map(|cave_name| Cave::from(cave_name))
        .collect::<Vec<Cave>>()
        .try_into()
        .unwrap();
    (a, b)
}

fn find_paths<'a, 'b>(
    graph: &'a GraphMap<Cave<'b>, HashSet<Cave<'b>>, Undirected>,
    allow_retread: bool,
) -> impl Iterator<Item = Vec<Cave<'b>>> + 'a {
    let mut visited = vec!["start"];
    let mut stack = vec![graph.neighbors("start")];
    let mut retread = None;

    from_fn(move || {
        while let Some(children) = stack.last_mut() {
            match children.next() {
                Some("end") => {
                    let path = visited
                        .iter()
                        .cloned()
                        .chain(Some("end"))
                        .collect::<Vec<Cave>>();
                    return Some(path);
                }
                Some(child) if is_big(child) || !visited.contains(&child) => {
                    visited.push(child);
                    stack.push(graph.neighbors(child));
                }
                Some(child) if is_small(child) && allow_retread && retread.is_none() => {
                    retread = Some(child);
                    visited.push(child);
                    stack.push(graph.neighbors(child));
                }
                Some(_) => (),
                None => {
                    match (retread, visited.pop()) {
                        (Some(a), Some(b)) if a == b => retread = None,
                        _ => (),
                    }
                    stack.pop();
                }
            }
        }
        None
    })
}
