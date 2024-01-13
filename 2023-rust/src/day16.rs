use std::{
    cmp::max,
    collections::{HashMap, HashSet},
    fs::read_to_string,
};

use itertools::{Itertools, MinMaxResult};
use num::Saturating;

enum Fixture {
    VerticalSplitter,
    HorizontalSplitter,
    AngleRight,
    AngleLeft,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
struct Photon {
    x: usize,
    y: usize,
    direction: Direction,
}

impl Photon {
    fn new(x: usize, y: usize, direction: Direction) -> Self {
        Self { x, y, direction }
    }

    fn next(self) -> Self {
        match self.direction {
            Direction::Up => Photon::new(self.x, self.y.saturating_add(1), Direction::Up),
            Direction::Down => Photon::new(self.x, self.y.saturating_sub(1), Direction::Down),
            Direction::Right => Photon::new(self.x.saturating_add(1), self.y, Direction::Right),
            Direction::Left => Photon::new(self.x.saturating_sub(1), self.y, Direction::Left),
        }
    }
}

impl Fixture {
    fn next(&self, curr: Photon) -> Vec<Photon> {
        match (self, curr.direction) {
            (Fixture::VerticalSplitter, Direction::Left | Direction::Right) => vec![
                Photon::new(curr.x, curr.y.saturating_add(1), Direction::Up),
                Photon::new(curr.x, curr.y.saturating_sub(1), Direction::Down),
            ],
            (Fixture::HorizontalSplitter, Direction::Up | Direction::Down) => vec![
                Photon::new(curr.x.saturating_add(1), curr.y, Direction::Right),
                Photon::new(curr.x.saturating_sub(1), curr.y, Direction::Left),
            ],
            (Fixture::AngleLeft, Direction::Up) | (Fixture::AngleRight, Direction::Down) => {
                vec![Photon::new(
                    curr.x.saturating_add(1),
                    curr.y,
                    Direction::Right,
                )]
            }
            (Fixture::AngleLeft, Direction::Down) | (Fixture::AngleRight, Direction::Up) => {
                vec![Photon::new(
                    curr.x.saturating_sub(1),
                    curr.y,
                    Direction::Left,
                )]
            }
            (Fixture::AngleLeft, Direction::Right) | (Fixture::AngleRight, Direction::Left) => {
                vec![Photon::new(curr.x, curr.y.saturating_add(1), Direction::Up)]
            }
            (Fixture::AngleLeft, Direction::Left) | (Fixture::AngleRight, Direction::Right) => {
                vec![Photon::new(
                    curr.x,
                    curr.y.saturating_sub(1),
                    Direction::Down,
                )]
            }
            (_, _) => {
                vec![curr.next()]
            }
        }
    }
}

fn read_cells(input: &str) -> HashMap<(usize, usize), Fixture> {
    input
        .lines()
        .enumerate()
        .map(|(i, row)| {
            println!("{i}");
            row.chars().enumerate().flat_map(move |(j, c)| match c {
                '.' => None,
                '/' => Some(((j, i), Fixture::AngleRight)),
                '\\' => Some(((j, i), Fixture::AngleLeft)),
                '|' => Some(((j, i), Fixture::VerticalSplitter)),
                '-' => Some(((j, i), Fixture::HorizontalSplitter)),
                _ => panic!(),
            })
        })
        .flatten()
        .collect::<HashMap<_, _>>()
}

pub fn part1() -> usize {
    let input = read_to_string("input/16").unwrap();
    // let input = "\
    // .|...\\....
    // |.-.\\.....
    // .....|-...
    // ........|.
    // ..........
    // .........\\
    // ..../.\\\\..
    // .-.-/..|..
    // .|....-|.\\
    // ..//.|....";
    let mut set = HashSet::<Photon>::new();

    let cells = read_cells(&input);

    let x_max = cells.keys().map(|(x, _y)| x).max().unwrap().clone();
    let y_max = cells.keys().map(|(_x, y)| y).max().unwrap().clone();

    let mut stack = vec![Photon {
        x: 0,
        y: 0,
        direction: Direction::Right,
    }];
    let mut i = 0;
    while let Some(photon) = stack.pop() {
        set.insert(photon);
        i += 1;
        println!("{photon:?}");
        let next_photons = if let Some(fixture) = cells.get(&(photon.x, photon.y)) {
            fixture.next(photon)
        } else {
            vec![photon.next()]
        };
        let asdf = next_photons
            .into_iter()
            .filter(|next_photon| {
                !(next_photon.x == photon.x && next_photon.y == photon.y)
                    && next_photon.x <= x_max
                    && next_photon.y <= y_max
                    && !set.contains(&next_photon)
            })
            .collect_vec();
        println!("Progressing to {asdf:?}");
        stack.extend(asdf);
        if i > 600 {
            // panic!();
        }
    }

    set.into_iter()
        .map(|photon| (photon.x, photon.y))
        .unique()
        .count()
}

pub fn part2() -> usize {
    let input = read_to_string("input/16").unwrap();
    // let input = "\
    // .|...\\....
    // |.-.\\.....
    // .....|-...
    // ........|.
    // ..........
    // .........\\
    // ..../.\\\\..
    // .-.-/..|..
    // .|....-|.\\
    // ..//.|....";

    let cells = read_cells(&input);
    let x_max = cells.keys().map(|(x, _y)| x).max().unwrap().clone();
    let y_max = cells.keys().map(|(_x, y)| y).max().unwrap().clone();

    let edges = (0..=y_max)
        .map(|y| Photon::new(0, y, Direction::Right))
        .chain((0..=y_max).map(|y| Photon::new(x_max, y, Direction::Left)))
        .chain((0..=x_max).map(|x| Photon::new(x, 0, Direction::Up)))
        .chain((0..=x_max).map(|x| Photon::new(x, y_max, Direction::Down)));

    edges
        .map(|photon| {
            let mut set = HashSet::<Photon>::new();
            let mut stack = vec![photon];
            while let Some(photon) = stack.pop() {
                set.insert(photon);
                let next_photons = if let Some(fixture) = cells.get(&(photon.x, photon.y)) {
                    fixture.next(photon)
                } else {
                    vec![photon.next()]
                };
                stack.extend(next_photons.into_iter().filter(|next_photon| {
                    !(next_photon.x == photon.x && next_photon.y == photon.y)
                        && next_photon.x <= x_max
                        && next_photon.y <= y_max
                        && !set.contains(&next_photon)
                }));
            }

            set.into_iter()
                .map(|photon| (photon.x, photon.y))
                .unique()
                .count()
        })
        .max()
        .unwrap()
}
