use std::{collections::hash_map::DefaultHasher, hash::Hasher};

use anyhow::Result;

#[derive(Clone, Copy, Debug)]
enum FoldInstruction {
    Horizontal(usize),
    Vertical(usize),
}

type Coordinate = (usize, usize);
type Paper = [[bool; 41]; 7];

impl From<&str> for FoldInstruction {
    fn from(line: &str) -> Self {
        let mut parts = line.split(" ").skip(2).next().unwrap().split("=");
        match (parts.next(), parts.next()) {
            (Some("x"), Some(x)) => FoldInstruction::Vertical(x.parse::<usize>().unwrap()),
            (Some("y"), Some(y)) => FoldInstruction::Horizontal(y.parse::<usize>().unwrap()),
            _ => panic!(),
        }
    }
}

fn follow_instructions(
    coords: Vec<Coordinate>,
    fold_instructions: Vec<FoldInstruction>,
) -> Vec<Coordinate> {
    fold_instructions
        .into_iter()
        .fold::<Box<dyn Iterator<Item = Coordinate>>, _>(
            Box::new(coords.into_iter()),
            fold_coordinates,
        )
        .collect()
}

fn fold_coordinates(
    coords: Box<dyn Iterator<Item = Coordinate>>,
    fold_instruction: FoldInstruction,
) -> Box<dyn Iterator<Item = Coordinate>> {
    Box::new(coords.map(move |coord| fold_coordinate(coord, fold_instruction)))
}

fn fold_coordinate((x, y): Coordinate, fold_instruction: FoldInstruction) -> Coordinate {
    match fold_instruction {
        FoldInstruction::Horizontal(n) => (x, fold_scalar(y, n)),
        FoldInstruction::Vertical(n) => (fold_scalar(x, n), y),
    }
}

fn fold_scalar(a: usize, n: usize) -> usize {
    if a >= n {
        2 * n - a
    } else {
        a
    }
}

fn parse_input(input: &str) -> (Vec<Coordinate>, Vec<FoldInstruction>) {
    let mut parts = input.split("\n\n");
    let coords: Vec<Coordinate> = parts.next().unwrap().split('\n').map(parse_line).collect();

    let fold_instructions: Vec<FoldInstruction> = parts
        .next()
        .unwrap()
        .split('\n')
        .map(FoldInstruction::from)
        .collect();
    (coords, fold_instructions)
}

fn parse_line(coord_str: &str) -> Coordinate {
    let [x, y]: [usize; 2] = coord_str
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect::<Vec<usize>>()
        .try_into()
        .unwrap();
    return (x, y);
}

fn part_1(coords: Vec<Coordinate>, fold_instructions: &Vec<FoldInstruction>) -> i64 {
    fold_coordinates(Box::new(coords.clone().into_iter()), fold_instructions[0]).count() as i64
}

fn part_2(coords: Vec<Coordinate>, fold_instructions: Vec<FoldInstruction>) -> String {
    let coords = follow_instructions(coords, fold_instructions);
    let paper = coords_to_paper(&coords);
    // print_paper(&paper);
    return read_paper(paper);
}

fn coords_to_paper(coords: &Vec<Coordinate>) -> [[bool; 40]; 6] {
    let mut paper = [[false; 40]; 6];
    for &(x, y) in coords {
        paper[y][x] = true;
    }
    paper
}

fn print_paper(paper: &[[bool; 40]; 6]) {
    for row in paper {
        let mut s = String::new();
        for y in row {
            if *y {
                s += "* ";
            } else {
                s += "  ";
            }
        }
        println!("{s}");
    }
}

fn read_dot_pattern_hash(hash: u64) -> char {
    match hash {
        0x6e0756323d2de145 => 'L',
        0x063247f195c29c9b => 'K',
        0x904bd89f482de97d => 'R',
        0x7533f69525f7227a => 'E',
        0x7916c81f2aa78713 => 'B',
        0x9dc009dd958c7511 => 'P',
        _ => panic!(),
    }
}

fn read_paper(paper: [[bool; 40]; 6]) -> String {
    (0..8)
        .map(|i| {
            let mut hasher = DefaultHasher::new();
            for j in 0..6 {
                for k in 0..4 {
                    hasher.write_u8(paper[j][5 * i + k].into());
                }
            }
            hasher.finish()
        })
        .map(read_dot_pattern_hash)
        .collect::<String>()
}

pub fn solve(input: &str) -> Result<()> {
    let (coords, fold_instructions) = parse_input(input);

    let soln_1 = part_1(coords.clone(), &fold_instructions);
    let soln_2 = part_2(coords, fold_instructions);

    Ok((Box::new(soln_1), Box::new(soln_2)))
}
