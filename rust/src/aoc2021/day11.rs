use crate::types::{BadInputError, SolveState};
use anyhow::Result;
use itertools::iproduct;

struct Day11 {
    state: SolveState,
}

pub fn solve(input: &str) -> Result<()> {
    let octopi = read_input(input)?;
    let mut state = OctopiFrame {
        flashes: 0,
        flashing: Vec::new(),
        octopi,
    };

    let mut step_counter = 0;
    let mut total_flashes = 0;
    while state.flashes < 10 * 10 {
        state.flashes = 0;
        state.step();
        step_counter += 1;
        if step_counter < 100 {
            total_flashes += state.flashes;
        }
    }
    Ok((Box::new(total_flashes), Box::new(step_counter)))
}

fn read_input(input: &str) -> Result<Vec<Vec<u32>>, BadInputError> {
    input.split("\n").map(read_line).collect()
}

fn read_line(line: &str) -> Result<Vec<u32>, BadInputError> {
    line.chars()
        .map(|c| c.to_digit(10))
        .collect::<Option<_>>()
        .ok_or(BadInputError(line.to_string()))
}

struct OctopiFrame {
    flashes: i64,
    flashing: Vec<(usize, usize)>,
    octopi: Vec<Vec<u32>>,
}

impl OctopiFrame {
    fn step(&mut self) {
        for row in self.octopi.iter_mut() {
            for octopus in row.iter_mut() {
                *octopus += 1;
            }
        }
        self.flash_octopi()
    }

    fn flash_octopi(&mut self) {
        for i in 0..self.octopi.len() {
            for j in 0..self.octopi[i].len() {
                self.maybe_flash_octopi(i, j)
            }
        }

        let m = self.octopi.len();
        let n = self.octopi[0].len();
        while let Some((x, y)) = self.flashing.pop() {
            // self.print_frame((x, y));
            for (i, j) in iter_surrounding((x, y), (m, n)) {
                if self.octopi[i][j] != 0 {
                    self.octopi[i][j] += 1;
                }
                self.maybe_flash_octopi(i, j);
            }
            // self.print_frame((x, y));
        }
    }

    fn maybe_flash_octopi(&mut self, i: usize, j: usize) {
        if self.octopi[i][j] > 9 {
            self.flashes += 1;
            self.flashing.push((i, j));
            self.octopi[i][j] = 0;
        }
    }

    fn print_frame(&self, (x, y): (usize, usize)) {
        let mut block = Vec::new();
        let surrounding: Vec<(usize, usize)> = self.iter_surrounding_octopi((x, y)).collect();

        for (i, row) in self.octopi.iter().enumerate() {
            for (j, &cell) in row.iter().enumerate() {
                if x == i && y == j {
                    block.push(format!("\u{001b}[34m{cell}\u{001b}[0m"));
                } else if surrounding.contains(&(i, j)) {
                    block.push(format!("\u{001b}[31m{cell}\u{001b}[0m"));
                } else if cell == 9 {
                    block.push(format!("\u{001b}[32m{cell}\u{001b}[0m"));
                } else {
                    block.push(cell.to_string());
                }
            }
            block.push("\n".to_string());
        }
        println!("{}", block.join(" "));
        // std::thread::sleep(std::time::Duration::from_secs(1));
    }

    fn iter_surrounding_octopi(
        &self,
        (x, y): (usize, usize),
    ) -> impl Iterator<Item = (usize, usize)> {
        let m = self.octopi.len();
        let n = self.octopi[0].len();
        iter_surrounding((x, y), (m, n))
    }
}

fn iter_surrounding(
    (x, y): (usize, usize),
    (m, n): (usize, usize),
) -> impl Iterator<Item = (usize, usize)> {
    let top = if x < m - 1 { x + 1 } else { x };
    let bottom = if x > 0 { x - 1 } else { x };
    let right = if y < n - 1 { y + 1 } else { y };
    let left = if y > 0 { y - 1 } else { y };
    iproduct!(bottom..=top, left..=right).filter(move |(i, j)| !(*i == x && *j == y))
}
