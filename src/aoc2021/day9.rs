use std::fs::read_to_string;

use crate::types::{ Solution, BadInputError};
use anyhow::Result;

type Coordinate = (usize, usize);

pub fn solve(path: &str) -> Result<(Solution, Solution)> {
    let height_map: Vec<Vec<u8>> = HeightMap::from_file(path)?;

    let bottom_coords = height_map.get_bottoms();
    let bottom_heights = bottom_coords
        .iter()
        .map(|&coord| height_map.get_height_at(coord).unwrap())
        .cloned()
        .collect();

    let soln1 = part1(bottom_heights);
    let soln2 = part2(bottom_coords, height_map);

    Ok((Ok(soln1), Ok(soln2)))
}

fn part1(bottom_heights: Vec<u8>) -> i64 {
    let sum_heights = bottom_heights.iter().map(|x| *x as i64).sum::<i64>();
    let cumulative_risk_level = bottom_heights.len() as i64;
    sum_heights + cumulative_risk_level
}

fn part2(bottom_coords: Vec<Coordinate>, height_map: Vec<Vec<u8>>) -> i64 {
    height_map
        .get_top_basin_sizes(bottom_coords)
        .iter()
        .product::<usize>() as i64
}

trait HeightMap<T>
where
    Self: Sized,
{
    fn from_file(path: &str) -> Result<Self>;
    fn get_height_at(&self, coord: Coordinate) -> Option<&u8>;
    fn get_mut_height_at(&mut self, coord: Coordinate) -> Option<&mut u8>;
    fn has_bottom_at(&self, coord: Coordinate) -> bool;
    fn get_bottoms(&self) -> Vec<Coordinate>;
    fn get_top_basin_sizes(&self, bottoms: Vec<Coordinate>) -> [usize; 3];
}

impl HeightMap<u8> for Vec<Vec<u8>> {
    fn from_file(path: &str) -> Result<Self> {
        read_to_string(path)?
            .split("\n")
            .filter(|&s| s != "")
            .map(read_line)
            .collect()
    }

    fn get_height_at(&self, (j, i): Coordinate) -> Option<&u8> {
        self.get(i)?.get(j)
    }

    fn get_mut_height_at(&mut self, (j, i): Coordinate) -> Option<&mut u8> {
        self.get_mut(i)?.get_mut(j)
    }

    fn get_bottoms(&self) -> Vec<Coordinate> {
        let mut low_points = Vec::new();
        for (y, row) in self.iter().enumerate() {
            for x in 0..row.len() {
                if self.has_bottom_at((x, y)) {
                    low_points.push((x, y));
                }
            }
        }
        low_points
    }

    fn has_bottom_at(&self, (x, y): Coordinate) -> bool {
        if let Some(height) = self.get_height_at((x, y)) {
            let neighbors = [
                x.checked_sub(1).map(|x| (x, y)), // above
                x.checked_add(1).map(|x| (x, y)), // below
                y.checked_sub(1).map(|y| (x, y)), // left
                y.checked_add(1).map(|y| (x, y)), // right
            ];

            let lower_neighbor = neighbors
                .into_iter()
                .flatten()
                .map(|coord| self.get_height_at(coord))
                .flatten()
                .find(|h| *h <= height);

            return lower_neighbor.is_none();
        } else {
            return false;
        }
    }

    fn get_top_basin_sizes(&self, bottom_coords: Vec<Coordinate>) -> [usize; 3] {
        let mut top_basin_sizes = [0; 3];
        let mut clone = self.clone();
        let count_filled = |clone: &Self| {
            clone
                .iter()
                .flatten()
                .filter(|&&height| height != 9)
                .count()
        };
        for bottom in bottom_coords {
            let before = count_filled(&clone);
            clone.fill(bottom);
            let after = count_filled(&clone);
            let curr_basin_size = before - after;

            for basin_size in top_basin_sizes.iter_mut() {
                if curr_basin_size > *basin_size {
                    *basin_size = curr_basin_size;
                    break;
                }
            }
        }
        top_basin_sizes
    }
}

impl Floodable<u8> for Vec<Vec<u8>> {
    fn is_target(&self, coord: Coordinate) -> bool {
        match self.get_height_at(coord) {
            Some(&height) => height != 9,
            None => false,
        }
    }

    fn color(&mut self, coord: Coordinate) {
        if let Some(height) = self.get_mut_height_at(coord) {
            *height = 9;
        }
    }
}

#[derive(Clone, Copy)]
enum ScanDirection {
    Up,
    Down,
}

impl ScanDirection {
    fn opposite(self) -> Self {
        match self {
            ScanDirection::Up => ScanDirection::Down,
            ScanDirection::Down => ScanDirection::Up,
        }
    }
    fn next_y(self, y: usize) -> Option<usize> {
        match self {
            ScanDirection::Up => y.checked_sub(1),
            ScanDirection::Down => y.checked_add(1),
        }
    }
}

trait Floodable<T> {
    fn is_target(&self, coord: Coordinate) -> bool;
    fn color(&mut self, coord: Coordinate);

    fn scan_and_color(
        &mut self,
        next_x: fn(usize, usize) -> Option<usize>,
        init_x: usize,
        y: usize,
    ) -> usize {
        let mut x = init_x;
        while let Some(nx) = next_x(x, 1).filter(|&nx| self.is_target((nx, y))) {
            x = nx;
            self.color((nx, y));
        }
        return x;
    }

    fn fill(&mut self, coord: Coordinate) {
        if !self.is_target(coord) {
            return;
        }

        let (x, y) = coord;

        // Start scanning up and down from the initial position.
        let mut stack = Vec::new();
        stack.push((x, x, y, ScanDirection::Up));
        if let Some(ny) = ScanDirection::Down.next_y(y) {
            stack.push((x, x, ny, ScanDirection::Down));
        }

        while let Some((mut x1, x2, y, dy)) = stack.pop() {
            // Fill/scan as far left as possible
            let mut x = x1;
            if self.is_target((x, y)) {
                while let Some(nx) = x.checked_sub(1).filter(|&nx| self.is_target((nx, y))) {
                    self.color((nx, y));
                    x = nx;
                }
            }

            // If the current scan has progressed further to the left than it's
            // parent scan, push a 'u-turn' scan in the opposite direction to the left.
            if x < x1 {
                let ndy = dy.opposite();
                if let Some(ny) = ndy.next_y(y) {
                    if let Some(nx1) = x1.checked_sub(1).filter(|&nx1| x < nx1) {
                        stack.push((x, nx1, ny, ndy));
                    } else if self.is_target((x1, y)) {
                        stack.push((x, x1, ny, ndy));
                    }
                }
            }

            // Fill/scan as far right as possible, within and past the parent
            // scan
            while x1 <= x2 {
                while self.is_target((x1, y)) {
                    self.color((x1, y));
                    x1 = x1.saturating_add(1);
                }

                // Push a scan in the current direction
                if let Some(ny) = dy.next_y(y) {
                    if let Some(nx1) = x1.checked_sub(1).filter(|&nx1| x < nx1) {
                        stack.push((x, nx1, ny, dy));
                    } else if self.is_target((x1, y)) {
                        stack.push((x, x1, ny, dy));
                    }
                }

                // If the current scan has progressed further to the right than it's
                // parent scan, push a 'u-turn' scan in the opposite direction to the right.
                if x1.saturating_sub(1) > x2 {
                    let ndy = dy.opposite();
                    if let Some(ny) = ndy.next_y(y) {
                        let nx2 = x2.saturating_add(1);
                        let nx1 = x1.saturating_sub(1);
                        stack.push((nx2, nx1, ny, ndy))
                    }
                }

                // Move past unfillable space, until the right end of the parent
                // scan has been reached
                x1 = x1.saturating_add(1);
                while x1 < x2 && !self.is_target((x1, y)) {
                    x1 = x1.saturating_add(1);
                }
                x = x1;
            }
        }
    }
}

fn read_line(line: &str) -> Result<Vec<u8>> {
    line.chars()
        .map(|c| {
            let x = c.to_digit(10).ok_or(BadInputError(c.to_string()))?;
            Ok(x as u8)
        })
        .collect::<Result<Vec<_>, _>>()
}

// Debugging print functions below

fn print_basin(h1: &Vec<Vec<u8>>, h2: Option<&Vec<Vec<u8>>>, bottom: Coordinate) {
    let (x, y) = bottom;
    let xr = 30;
    let yr = 6;
    let x1 = x.checked_sub(xr).unwrap_or(0);
    let x2 = x
        .checked_add(xr)
        .filter(|x| *x <= h1[0].len())
        .unwrap_or(h1.len());
    let y1 = y.checked_sub(yr).unwrap_or(0);
    let y2 = y
        .checked_add(yr)
        .filter(|y| *y <= h1.len())
        .unwrap_or(h1.len());
    for i in y1..y2 {
        print!("{i:02} | ");
        for j in x1..x2 {
            if *h1.get_height_at((j, i)).unwrap() == 9 {
                print!("  ");
            } else if j == x && i == y {
                print!("\u{001b}[31m{}\u{001b}[0m ", h1[i][j]);
            } else if let Some(clone) = h2 {
                if h1[i][j] != clone[i][j] {
                    print!("\u{001b}[32m{}\u{001b}[0m ", h1[i][j]);
                } else {
                    print!("{} ", h1[i][j]);
                }
            } else {
                print!("{} ", h1[i][j]);
            }
        }
        println!("");
    }
}

fn print_cavern(h1: &Vec<Vec<u8>>, h2: Option<&Vec<Vec<u8>>>, bs: Option<&Vec<Coordinate>>) {
    println!("");
    print!("  ");
    for j in (0..h1[0].len()).step_by(2) {
        print!("{j:#4}")
    }
    println!("");
    for i in 0..h1.len() {
        print!("{i:#2} | ");
        for j in 0..h1[i].len() {
            if let Some(bs) = bs {
                if bs.contains(&(j, i)) {
                    print!("\u{001b}[32m*\u{001b}[0m ");
                    continue;
                }
            }
            if let Some(clone) = h2 {
                if h1[i][j] != clone[i][j] {
                    print!("\u{001b}[34m{}\u{001b}[0m ", h1[i][j]);
                    continue;
                }
            }
            if h1[i][j] == 9 {
                print!("  ");
            } else {
                print!("{} ", h1[i][j]);
            }
        }
        println!("");
    }
}
