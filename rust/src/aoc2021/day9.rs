use crate::types::{BadInputError, SolveState, Solver};
use anyhow::Result;
use itertools::iproduct;
use std::fmt::Debug;

type Coordinate = (usize, usize);
struct Day9 {
    state: SolveState,
    height_map: HeightMap,
    bottom_coords: Vec<(usize, usize)>,
}

impl Solver<'_> for Day9 {
    type Soln1 = i64;
    fn solve_part1(&mut self) -> Self::Soln1 {
        let bottom_heights: Vec<_> = self
            .bottom_coords
            .iter()
            .map(|&coord| self.height_map.get_height_at(coord))
            .flatten()
            .collect();
        let sum_heights = bottom_heights.iter().map(|x| *x as i64).sum::<i64>();
        let cumulative_risk_level = bottom_heights.len() as i64;
        sum_heights + cumulative_risk_level
    }

    type Soln2 = usize;
    fn solve_part2(&mut self) -> Self::Soln2 {
        get_top_basin_sizes(&self.bottom_coords, self.height_map)
            .into_iter()
            .product::<usize>()
    }
}

impl TryFrom<&str> for Day9 {
    type Error = BadInputError;

    fn try_from(value: &str) -> std::result::Result<Self, Self::Error> {
        let height_map = HeightMap::try_from(value)?;
        let bottom_coords = height_map.iter_bottoms().collect();

        Ok(Day9 {
            state: SolveState::new(),
            height_map,
            bottom_coords,
        })
    }
}

fn part1(bottom_heights: Vec<u8>) -> i64 {
    let sum_heights = bottom_heights.iter().map(|x| *x as i64).sum::<i64>();
    let cumulative_risk_level = bottom_heights.len() as i64;
    sum_heights + cumulative_risk_level
}

fn get_surrounding_coords((x, y): Coordinate) -> [Option<Coordinate>; 4] {
    [
        (x.checked_add(1), Some(y)),
        (x.checked_sub(1), Some(y)),
        (Some(x), y.checked_add(1)),
        (Some(x), y.checked_sub(1)),
    ]
    .map(|(x, y)| Some((x?, y?)))
}

fn get_top_basin_sizes(bottom_coords: &Vec<Coordinate>, mut height_map: HeightMap) -> [usize; 3] {
    let mut top_basin_sizes = [0; 3];
    for &bottom in bottom_coords {
        let curr = height_map.get_basin_size(bottom);

        for basin_size in top_basin_sizes.iter_mut() {
            if curr > *basin_size {
                *basin_size = curr;
                break;
            }
        }
    }
    top_basin_sizes
}

struct HeightMap {
    map: Vec<u8>,
    width: usize,
}

impl TryFrom<&str> for HeightMap {
    type Error = BadInputError;

    fn try_from(value: &str) -> std::result::Result<Self, Self::Error> {
        let map: Vec<Vec<_>> = value.split("\n").map(read_line).collect::<Result<_, _>>()?;
        let height = map.len();
        let map: Vec<_> = map.into_iter().flatten().collect();
        let width = map.len() / height;
        Ok(HeightMap { map, width })
    }
}

impl HeightMap {
    fn get_height_at(&self, (x, y): Coordinate) -> Option<u8> {
        self.map.get(y * self.width + x).cloned()
    }

    fn get_height_at_mut(&self, (x, y): Coordinate) -> Option<&mut u8> {
        self.map.get_mut(y * self.width + x)
    }

    fn iter_bottoms<'a>(&'a self) -> impl Iterator<Item = Coordinate> + 'a {
        let height = self.map.len() / self.width;
        iproduct!(0..self.width, 0..height).filter(move |coord| self.has_bottom_at(*coord))
    }

    fn has_bottom_at(&self, (x, y): Coordinate) -> bool {
        if let Some(h1) = self.get_height_at((x, y)) {
            get_surrounding_coords((x, y))
                .into_iter()
                .flatten()
                .flat_map(|coord| self.get_height_at(coord))
                .all(|h2| h1 <= h2)
        } else {
            false
        }
    }

    fn count_filled(&self) -> usize {
        self.map.iter().filter(|&&height| height != 9).count()
    }

    fn get_basin_size(&mut self, coord: Coordinate) -> usize {
        let before = self.count_filled();
        self.flood(coord);
        let after = self.count_filled();

        return before - after;
    }
}

impl Floodable<u8> for HeightMap {
    fn is_target(&self, coord: Coordinate) -> bool {
        match self.get_height_at(coord) {
            Some(height) => height != 9,
            None => false,
        }
    }

    fn color(&mut self, coord: Coordinate) {
        if let Some(height) = self.get_height_at_mut(coord) {
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
    fn next_y(self, y: usize) -> Option<usize> {
        match self {
            ScanDirection::Up => y.checked_sub(1),
            ScanDirection::Down => y.checked_add(1),
        }
    }
    fn reverse(self) -> ScanDirection {
        match self {
            ScanDirection::Up => ScanDirection::Down,
            ScanDirection::Down => ScanDirection::Up,
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

    fn flood(&mut self, coord: Coordinate) {
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
                let ndy = dy.reverse();
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
                    let ndy = dy.reverse();
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

// Debugging print functions below

fn print_basin(
    h1: &Vec<Vec<u8>>,
    h2: Option<&Vec<Vec<u8>>>,
    x_radius: Option<usize>,
    y_radius: Option<usize>,
    bottom: Coordinate,
) {
    let (x, y) = bottom;

    let x_radius = x_radius.unwrap_or(30);
    let y_radius = y_radius.unwrap_or(6);

    let x1 = x.checked_sub(x_radius).unwrap_or(0);
    let y1 = y.checked_sub(y_radius).unwrap_or(0);

    let x2 = x
        .checked_add(x_radius)
        .filter(|&x| x <= h1[0].len())
        .unwrap_or(h1.len());
    let y2 = y
        .checked_add(y_radius)
        .filter(|&y| y <= h1.len())
        .unwrap_or(h1.len());

    for i in y1..y2 {
        print!("{i:02} | ");
        for j in x1..x2 {
            if h1[i][j] == 9 {
                print!(" ")
            } else if j == x && i == y {
                print!("\u{001b}[31m{}\u{001b}[0m ", h1[i][j])
            } else if let Some(clone) = h2 && clone[i][j] != h1[i][j] {
                print!("\u{001b}[32m{}\u{001b}[0m ", h1[i][j])
            } else {
                print!("{} ", h1[i][j])
            };
        }
        println!("");
    }
}

fn print_cavern(h1: &Vec<Vec<u8>>, h2: Option<&Vec<Vec<u8>>>, basin: Option<&Vec<Coordinate>>) {
    println!("");
    print!("  ");
    for j in (0..h1[0].len()).step_by(2) {
        print!("{j:#4}")
    }
    println!("");
    for i in 0..h1.len() {
        print!("{i:#2} | ");
        for j in 0..h1[i].len() {
            if let Some(b) = basin && b.contains(&(j, i)) {
                print!("\u{001b}[32m*\u{001b}[0m ");
            } else if let Some(clone) = h2 && clone[i][j] != h1[i][j] {
                print!("\u{001b}[34m{}\u{001b}[0m ", h1[i][j]);
            } else if h1[i][j] == 9 {
                print!("  ");
            } else {
                print!("{} ", h1[i][j]);
            }
        }
        println!("");
    }
}

fn read_line(line: &str) -> Result<Vec<u8>, BadInputError> {
    line.chars()
        .map(|c| {
            c.to_digit(10)
                .map(|x| x as u8)
                .ok_or(BadInputError(c.to_string()))
        })
        .collect()
}

impl Iterator for Day9 {
    type Item = Box<dyn Debug>;

    fn next(&mut self) -> Option<Self::Item> {
        self.state.next()
    }
}
