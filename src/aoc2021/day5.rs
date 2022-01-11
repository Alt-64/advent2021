// https://adventofcode.com/2021/day/5
use std::{fmt::Debug, fs::read_to_string};

use crate::types::{Error, Solution};

pub fn solver(path: &str) -> Result<(Solution, Solution), Error> {
    let lines: Vec<Line> = read_input(path)?;

    let mut x_max = 0;
    let mut y_max = 0;
    for line in lines.iter() {
        if line.x_upper > x_max {
            x_max = line.x_upper
        }
        if line.y_upper > y_max {
            y_max = line.y_upper
        }
    }

    let mut canvas = vec![vec![0; (y_max + 1) as usize]; (x_max + 1) as usize];

    let soln1 = part1(&lines, &mut canvas);
    let soln2 = part2(&lines, &mut canvas);

    Ok((Ok(soln1), Ok(soln2)))
}

fn part1(lines: &Vec<Line>, canvas: &mut Vec<Vec<u8>>) -> i64 {
    for line in lines.iter() {
        line.draw_horiz(canvas);
        line.draw_vert(canvas);
    }
    count_overlaps(canvas)
}

fn part2(lines: &Vec<Line>, canvas: &mut Vec<Vec<u8>>) -> i64 {
    for line in lines.iter() {
        line.draw_diag(canvas);
    }
    count_overlaps(canvas)
}

fn count_overlaps(canvas: &Vec<Vec<u8>>) -> i64 {
    canvas
        .iter()
        .flatten()
        .filter(|&&overlap_count| overlap_count >= 2)
        .count() as i64
}

fn read_input(path: &str) -> Result<Vec<Line>, Error> {
    read_to_string(path)?
        .split("\n")
        .filter(|&s| s != "")
        .map(read_input_line)
        .collect()
}

fn read_input_line(line_str: &str) -> Result<Line, Error> {
    let mut points = line_str.split(" -> ").map(read_input_point).take(2);

    let a = points
        .next()
        .ok_or(Error::Malformed(line_str.to_owned()))??;
    let b = points
        .next()
        .ok_or(Error::Malformed(line_str.to_owned()))??;

    Ok(Line::from_points(a, b))
}

fn read_input_point(point_str: &str) -> Result<Point, Error> {
    let mut coords = point_str.split(',').map(str::parse::<i64>);

    let x = coords
        .next()
        .ok_or(Error::Malformed(point_str.to_owned()))??;
    let y = coords
        .next()
        .ok_or(Error::Malformed(point_str.to_owned()))??;

    Ok(Point { x, y })
}

#[derive(Clone, Copy)]
struct Point {
    x: i64,
    y: i64,
}

struct Line {
    x_lower: i64,
    x_upper: i64,
    y_lower: i64,
    y_upper: i64,
    a: i64,
    b: i64,
    c: i64,
}

impl Debug for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}<=x<={}, {}<=y<={} | {}x + {}y + {} = 0",
            self.x_lower, self.x_upper, self.y_lower, self.y_upper, self.a, self.b, self.c,
        ))
    }
}

impl Line {
    fn from_points(p1: Point, p2: Point) -> Self {
        let mut x_bounds = [p1.x, p2.x];
        x_bounds.sort();
        let mut y_bounds = [p1.y, p2.y];
        y_bounds.sort();

        Line {
            x_lower: x_bounds[0],
            x_upper: x_bounds[1],
            y_lower: y_bounds[0],
            y_upper: y_bounds[1],
            a: p1.y - p2.y,
            b: p2.x - p1.x,
            c: p1.y * (p1.x - p2.x) + p1.x * (p2.y - p1.y),
        }
    }

    fn solve_for_x(&self, y: i64) -> i64 {
        -(self.b * y + self.c) / self.a
    }

    fn solve_for_y(&self, x: i64) -> i64 {
        -(self.a * x + self.c) / self.b
    }

    fn draw_vert(&self, canvas: &mut Vec<Vec<u8>>) {
        if self.x_lower == self.x_upper {
            let x = self.solve_for_x(self.y_lower);
            for y in self.y_lower..self.y_upper + 1 {
                canvas[x as usize][y as usize] += 1;
            }
        }
    }

    fn draw_horiz(&self, canvas: &mut Vec<Vec<u8>>) {
        if self.y_lower == self.y_upper {
            let y = self.solve_for_y(self.x_lower);
            for x in self.x_lower..self.x_upper + 1 {
                canvas[x as usize][y as usize] += 1;
            }
        }
    }

    fn draw_diag(&self, canvas: &mut Vec<Vec<u8>>) {
        if self.x_lower != self.x_upper && self.y_lower != self.y_upper {
            let mut y;
            for x in self.x_lower..self.x_upper + 1 {
                y = self.solve_for_y(x);
                canvas[x as usize][y as usize] += 1;
            }
        }
    }
}
