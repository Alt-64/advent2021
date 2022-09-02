use std::fs::read_to_string;

use crate::types::{Answer, BadInputError};
use anyhow::Result;

pub fn solve(path: &str) -> Result<(Answer, Answer)> {
    let octopi = read_to_string(path)?
        .split("\n")
        .filter(|&s| s != "")
        .map(read_line)
        .collect::<Result<Vec<_>, _>>()?;
    let mut state = OctopiFrame::<10, 10> {
        flashes: 0,
        flashing: Vec::new(),
        octopi: devec(octopi)?,
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

fn read_line(line: &str) -> Result<Vec<u32>> {
    line.chars()
        .map(|c| c.to_digit(10).ok_or_else(|| BadInputError(c.to_string())))
        .collect()
}

fn devec<T, const N: usize, const M: usize>(vecs: Vec<Vec<T>>) -> Result<[[T; M]; N]> {
    vecs.into_iter()
        .map(TryInto::try_into)
        .collect::<Result<Vec<_>>>()?
        .try_into()
}

struct OctopiFrame<const N: usize, const M: usize> {
    flashes: i64,
    flashing: Vec<(usize, usize)>,
    octopi: [[u32; M]; N],
}

impl<const N: usize, const M: usize> OctopiFrame<N, M> {
    fn step(&mut self) -> &mut Self {
        for i in 0..N {
            for j in 0..M {
                self.octopi[i][j] += 1;
            }
        }
        self.flash_octopi()
    }

    fn maybe_flash_octopus(&mut self, i: usize, j: usize) {
        if self.octopi[i][j] > 9 {
            self.flashes += 1;
            self.flashing.push((i, j));
            self.octopi[i][j] = 0;
        }
    }

    fn flash_octopi(&mut self) -> &mut Self {
        for i in 0..N {
            for j in 0..M {
                self.maybe_flash_octopus(i, j);
            }
        }
        while let Some((x, y)) = self.flashing.pop() {
            let top = if y < M - 1 { y + 1 } else { y };
            let bottom = if y > 0 { y - 1 } else { y };
            let right = if x < N - 1 { x + 1 } else { x };
            let left = if x > 0 { x - 1 } else { x };

            for i in left..=right {
                for j in bottom..=top {
                    if i == x && j == y {
                        continue;
                    }
                    if self.octopi[i][j] != 0 {
                        self.octopi[i][j] += 1;
                        self.maybe_flash_octopus(i, j);
                    }
                }
            }
        }

        self
    }
}
