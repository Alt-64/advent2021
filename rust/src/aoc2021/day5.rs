// https://adventofcode.com/2021/day/5

use crate::types::{BadInputError, NoSolutionError, SolveState, Solver};
use anyhow::Result;
use std::fmt::Debug;
use std::num::ParseIntError;

pub struct Day5 {
    state: SolveState,
    canvas: Canvas,
}

impl TryFrom<&str> for Day5 {
    type Error = anyhow::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let lines = value
            .split("\n")
            .map(read_input_line)
            .collect::<Result<_, _>>()?;
        let (x_max, y_max) = get_canvas_size(&lines)?;

        Ok(Day5 {
            state: SolveState::new(),
            canvas: lines
                .into_iter()
                .fold(Canvas::new(x_max, y_max), Canvas::draw_line),
        })
    }
}

impl Solver<'_> for Day5 {
    type Soln1 = usize;
    fn solve_part1(&mut self) -> Self::Soln1 {
        self.canvas
            .pixels
            .into_iter()
            .map(|(straights, _)| straights)
            .filter(|&overlap_count| overlap_count >= 2)
            .count()
    }

    type Soln2 = usize;
    fn solve_part2(&mut self) -> Self::Soln2 {
        self.canvas
            .pixels
            .into_iter()
            .map(|(_, all)| all)
            .filter(|&overlap_count| overlap_count >= 2)
            .count()
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

struct Canvas {
    pixels: Vec<(u8, u8)>,
    width: usize,
    height: usize,
}

impl Canvas {
    fn new(width: usize, height: usize) -> Canvas {
        Canvas {
            pixels: vec![(0, 0); (height + 1) * (width + 1) as usize],
            width,
            height,
        }
    }

    fn draw_line(self, line: Line) -> Self {
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

    fn draw_pixel_l(self, x: usize, y: usize) -> Self {
        self.pixels[y * self.height + x].0 += 1;
        self
    }

    fn draw_pixel_r(self, x: usize, y: usize) -> Self {
        self.pixels[y * self.height + x].1 += 1;
        self
    }

    fn draw_horiz(self, x1: usize, y: usize, x2: usize) -> Self {
        for i in x1..x2 {
            self = self.draw_pixel_l(i, y);
        }
        self
    }

    fn draw_vert(self, x: usize, y1: usize, y2: usize) -> Self {
        for j in y1..y2 {
            self = self.draw_pixel_l(x, j);
        }
        self
    }

    fn draw_diag(self, Point { x: x1, y: y1 }: Point, Point { x: x2, y: y2 }: Point) -> Self {
        for (i, j) in (x1..x2).zip(y1..y2) {
            self = self.draw_pixel_l(i, j);
            self = self.draw_pixel_r(i, j);
        }
        self
    }
}
impl Iterator for Day5 {
    type Item = Box<dyn Debug>;
    fn next(&mut self) -> Option<Box<dyn Debug>> {
        self.state.next()
    }
}
