use crate::types::{Answer, BadInputError};
use anyhow::Result;

pub fn solve(input: &str) -> Result<(Answer, Answer)> {
    let octopi = read_input(input)?;
    let mut state = OctopiFrame {
        flashes: 0,
        flashing: Vec::new(),
        octopi,
    };

    let mut step_counter = 0;
    let mut total_flashes = 0;
    while state.flashes != 10 * 10 {
        state.flashes = 0;
        state.step();
        step_counter += 1;
        if step_counter < 100 {
            total_flashes += state.flashes;
        }
    }
    Ok((Ok(total_flashes), Ok(step_counter)))
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

        while let Some((x, y)) = self.flashing.pop() {
            let m = self.octopi.len();
            let n = self.octopi[0].len();

            let top = if y < m - 1 { y + 1 } else { y };
            let bottom = if y > 0 { y - 1 } else { y };
            let right = if x < n - 1 { x + 1 } else { x };
            let left = if x > 0 { x - 1 } else { x };

            for i in bottom..=top {
                for j in left..=right {
                    if i == x && j == y {
                        continue;
                    }
                    if self.octopi[i][j] != 0 {
                        self.octopi[i][j] += 1;
                        self.maybe_flash_octopi(i, j);
                    }
                }
            }
        }
    }

    fn maybe_flash_octopi(&mut self, i: usize, j: usize) {
        if let Some(flashing_octopus) = maybe_flash_octopus(self.octopi[j][i]) {
            self.flashes += 1;
            self.flashing.push((i, j));
            self.octopi[j][i] = flashing_octopus;
        }
    }
}

fn maybe_flash_octopus(octopus: u32) -> Option<u32> {
    if octopus > 9 {
        Some(0)
    } else {
        None
    }
}
