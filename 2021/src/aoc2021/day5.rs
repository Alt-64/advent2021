// https://adventofcode.com/2021/day/5

use std::num::ParseIntError;
use crate::types::{BadInputError, NoSolutionError, Solver};
use anyhow::Result;

struct Day5(Vec<(u8, u8)>);

impl Solver for Day5 {
    type Soln1 = usize;
    fn solve_part1(&self) -> Result<Self::Soln1> {
        Ok(count_overlaps(self.0, |(straights, _)| straights))
    }

    type Soln2 = usize;
    fn solve_part2(&self) -> Result<Self::Soln2> {
        Ok(count_overlaps(self.0, |(_, all)| all))
    }
}

fn count_overlaps<'a>(canvas: Vec<(u8, u8)>, splitter: fn((u8, u8)) -> u8) -> usize {
    canvas.into_iter().map(splitter).filter(|&overlap_count| overlap_count >= 2).count()
}


impl TryFrom<&str> for Day5 {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let lines = value
        .split("\n")
        .map(read_input_line)
        .collect::<Result<_, _>>()?;
        let (x_max, y_max) = get_canvas_size(&lines)?;

        let mut canvas: Vec<Vec<(u8, u8)>> = Canvas::new(x_max, y_max)?;
        for line in lines {
            canvas.draw(line);
        }

        Ok(Day5(canvas.into_iter().flatten().collect()))
    }
}

fn read_input_line<'a>(line_str: &'a str) -> Result<Line> {
    let [a, b]: [Point; 2] = line_str
        .split(" -> ")
        .map(read_input_point)
        .collect::<Result<Vec<Point>>>()?
        .try_into()
        .or(Err(BadInputError(line_str.to_string())))?;
    Ok((a, b))
}

fn read_input_point<'a>(point_str: &'a str) -> Result<Point> {
    let [x, y]: [usize; 2] = point_str
        .split(",")
        .map(str::parse::<usize>)
        .collect::<Result<Vec<_>, ParseIntError>>()?
        .try_into()
        .or(Err(BadInputError(point_str.to_string())))?;
    Ok(Point { x, y })
}

struct Point {
    x: usize,
    y: usize,
}

type Line = (Point, Point);

fn get_canvas_size(lines: &Vec<Line>) -> Result<(usize, usize)> {
    let (xs, ys): (Vec<usize>, Vec<usize>) = lines
        .iter()
        .flat_map(|(a, b)| [a, b])
        .map(|Point { x, y }| (x, y))
        .unzip();

    let x_max = xs.into_iter().max().ok_or(NoSolutionError)?;
    let y_max = ys.into_iter().max().ok_or(NoSolutionError)?;

    Ok((x_max, y_max))
}

trait Canvas
where
    Self: Sized,
{
    fn new(x_max: usize, y_max: usize) -> Result<Self>;
    fn draw(&mut self, line: Line);
    fn draw_horiz(&mut self, x1: usize, y1: usize, x2: usize);
    fn draw_vert(&mut self, x1: usize, y1: usize, y2: usize);
    fn draw_diag(&mut self, a: Point, b: Point);
}

impl Canvas for Vec<Vec<(u8, u8)>> {
    fn new(x_max: usize, y_max: usize) -> Result<Vec<Vec<(u8, u8)>>> {
        let canvas = vec![vec![(0, 0); (y_max + 1) as usize]; (x_max + 1) as usize];

        Ok(canvas)
    }

    fn draw(&mut self, line: Line) {
        match line {
            (Point { x: x1, y: y1 }, Point { x: x2, y: _ }) if x1 == x2 => {
                self.draw_horiz(x1, y1, x2)
            }
            (Point { x: x1, y: y1 }, Point { x: _, y: y2 }) if y1 == y2 => {
                self.draw_vert(x1, y1, y2)
            }
            (a, b) => self.draw_diag(a, b),
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

    fn draw_diag(&mut self, Point { x: x1, y: y1 }: Point, Point { x: x2, y: y2 }: Point) {
        let xs = x1..x2;
        let ys = y1..y2;
        for (i, j) in xs.zip(ys) {
            self[i][j].0 += 1;
            self[i][j].1 += 1;
        }
    }
}
