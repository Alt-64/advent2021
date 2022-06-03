// https://adventofcode.com/2021/day/5
use std::fs::read_to_string;

use crate::types::{ Solution, NoSolutionError, BadInputError};
use anyhow::Result;

type Point = [usize; 2];
type Line = [Point; 2];

pub fn solve(path: &str) -> Result<(Solution, Solution)> {
    let lines: Vec<Line> = read_input(path)?;
    let (xs, ys): (Vec<usize>, Vec<usize>) = lines.iter().flatten().map(|[x, y]| (x, y)).unzip();

    let x_max = xs.into_iter().max().ok_or(NoSolutionError)?;
    let y_max = ys.into_iter().max().ok_or(NoSolutionError)?;

    let mut canvas: Vec<Vec<(u8, u8)>> = Canvas::new(x_max, y_max)?;
    canvas.draw(lines);

    let soln1 = Ok(part1(&canvas));
    let soln2 = Ok(part2(&canvas));

    Ok((soln1, soln2))
}

trait Canvas
where
    Self: Sized,
{
    fn new(x_max: usize, y_max: usize) -> Result<Self>;
    fn draw(&mut self, lines: Vec<Line>);
    fn draw_horiz(&mut self, x1: usize, y1: usize, x2: usize);
    fn draw_vert(&mut self, x1: usize, y1: usize, y2: usize);
    fn draw_diag(&mut self, a: Point, b: Point);
}

impl Canvas for Vec<Vec<(u8, u8)>> {
    fn new(x_max: usize, y_max: usize) -> Result<Vec<Vec<(u8, u8)>>> {
        Ok(vec![
            vec![(0, 0); (y_max + 1) as usize];
            (x_max + 1) as usize
        ])
    }

    fn draw(&mut self, lines: Vec<Line>) {
        for line in lines {
            match line {
                [[x1, y1], [x2, _]] if x1 == x2 => self.draw_horiz(x1, y1, x2),
                [[x1, y1], [_, y2]] if y1 == y2 => self.draw_vert(x1, y1, y2),
                [a, b] => self.draw_diag(a, b),
            }
        }
    }

    fn draw_horiz(&mut self, x1: usize, y1: usize, x2: usize) {
        for i in x1..x2 {
            self[i][y1].0 += 1;
        }
    }

    fn draw_vert(&mut self, x1: usize, y1: usize, y2: usize) {
        for i in y1..y2 {
            self[x1][i].0 += 1;
        }
    }

    fn draw_diag(&mut self, [x1, y1]: Point, [x2, y2]: Point) {
        let xs = x1..x2;
        let ys = y1..y2;
        for (i, j) in xs.zip(ys) {
            self[i][j].0 += 1;
            self[i][j].1 += 1;
        }
    }
}

fn part1(canvas: &Vec<Vec<(u8, u8)>>) -> i64 {
    count_overlaps(canvas.iter().flatten().map(|(straights, _all)| straights))
}

fn part2(canvas: &Vec<Vec<(u8, u8)>>) -> i64 {
    count_overlaps(canvas.iter().flatten().map(|(_straights, all)| all))
}

fn count_overlaps<'a>(canvas: impl Iterator<Item = &'a u8>) -> i64 {
    canvas.filter(|&&overlap_count| overlap_count >= 2).count() as i64
}

fn read_input(path: &str) -> Result<Vec<Line>> {
    read_to_string(path)?
        .split("\n")
        .filter(|&s| s != "")
        .map(read_input_line)
        .collect()
}

fn read_pair<T>(
    pair_str: &str,
    pat: &str,
    read: fn(&str) -> Result<T>,
) -> Result<[T; 2]> {
    pair_str
        .split(pat)
        .map(read)
        .collect::<Result<Vec<_>, _>>()?
        .try_into() // Try to get exactly two
}

fn read_input_line(line_str: &str) -> Result<Line> {
    read_pair(line_str, " -> ", read_input_point)
}

fn read_input_point(point_str: &str) -> Result<Point> {
    read_pair(point_str, ",", str::parse::<usize>)
}
