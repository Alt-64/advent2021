use std::{
    collections::{HashMap, HashSet},
    fs::read_to_string,
    hash::{DefaultHasher, Hash, Hasher},
};

use itertools::Itertools;

pub fn part1() -> usize {
    let mut blocker = [SIZE; SIZE];
    let mut stones = [0; SIZE];
    let mut weight = 0;
    for (i, line) in read_to_string("input/14").unwrap().lines().enumerate() {
    // for (i, line) in TEST.lines().enumerate() {
        for (j, c) in line.chars().enumerate() {
            match c {
                '.' => (),
                'O' => {
                    stones[j] += 1;
                }
                '#' => {
                    weight += (0..stones[j]).map(|r| blocker[j] - r).sum::<usize>();
                    blocker[j] = SIZE - i - 1;
                    stones[j] = 0;
                }
                _ => panic!(),
            }
        }
    }
    for j in 0..SIZE {
        weight += (0..stones[j]).map(|r| blocker[j] - r).sum::<usize>();
    }
    weight
}

pub fn part1_v2() -> usize {
    const ARRAY_REPEAT_VALUE: Vec<usize> = Vec::new();
    let mut x_blockers = [ARRAY_REPEAT_VALUE; SIZE];
    let mut y_blockers = [ARRAY_REPEAT_VALUE; SIZE];
    let mut stones_curr = [ARRAY_REPEAT_VALUE; SIZE];
    let mut stones_next = [ARRAY_REPEAT_VALUE; SIZE];
    for (y, line) in read_to_string("input/14").unwrap().lines().enumerate() {
    // for (y, line) in TEST.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            match c {
                '.' => (),
                'O' => {
                    stones_curr[x].push(y);
                }
                '#' => {
                    x_blockers[x].push(y);
                    y_blockers[y].push(x);
                }
                _ => panic!(),
            }
        }
    }

    for (y0, line) in y_blockers.iter().enumerate().rev() {
        for x0 in line {
            for y2 in 1..=stones_curr[*x0].extract_if(|y1| y0 < *y1).count() {
                stones_next[y0 + y2].push(*x0);
            }
        }
    }
    for x0 in 0..SIZE {
        for y2 in 0..stones_curr[x0].len() {
            stones_next[y2].push(x0)
        }
    }
    calc_weight(&stones_next, true)
}

fn print(blockers: &[Vec<usize>], stones: &[Vec<usize>], flip: bool) {
    let mut base = [['.'; SIZE]; SIZE];
    for (i, blockers) in blockers.iter().enumerate() {
        for j in blockers {
            base[i][*j] = '#'
        }
    }

    for (i, stones) in stones.iter().enumerate() {
        for j in stones {
            if flip {
                base[i][*j] = 'O'
            } else {
                base[*j][i] = 'O'
            }
        }
    }
    println!("{}", base.map(|row| String::from_iter(row)).join("\n"));
}

fn calc_weight(stones: &[Vec<usize>], flip: bool) -> usize {
    let mut weight = 0;
    // i is x, j is y
    for (i, stones) in stones.iter().enumerate() {
        for j in stones {
            if flip {
                weight += SIZE - i;
            } else {
                weight += SIZE - j;
            }
            println!("{weight} <- {} | {i},{j} ", SIZE - i);
        }
    }
    weight
}

const SIZE: usize = 100;
const CYCLES: usize = 1000000000;
const TEST: &str = "\
OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#....";

pub fn part2() -> usize {
    const ARRAY_REPEAT_VALUE: Vec<usize> = Vec::new();
    let mut x_blockers = [ARRAY_REPEAT_VALUE; SIZE];
    let mut y_blockers = [ARRAY_REPEAT_VALUE; SIZE];
    let mut stones_curr = [ARRAY_REPEAT_VALUE; SIZE];
    let mut stones_next = [ARRAY_REPEAT_VALUE; SIZE];
    let mut states = Vec::<u64>::new();
    let mut loads = Vec::<usize>::new();
    for (y, line) in read_to_string("input/14").unwrap().lines().enumerate() {
    // for (y, line) in TEST.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            match c {
                '.' => (),
                'O' => {
                    stones_curr[x].push(y);
                }
                '#' => {
                    x_blockers[x].push(y);
                    y_blockers[y].push(x);
                }
                _ => panic!(),
            }
        }
    }

    println!("-1, {}", calc_weight(&stones_curr, true));
    let (loop_start, loop_end) = (0..CYCLES)
        .find_map(|curr_cycle| {
            // Roll North
            for (y0, line) in y_blockers.iter().enumerate().rev() {
                for x0 in line {
                    for y2 in 1..=stones_curr[*x0].extract_if(|y1| y0 < *y1).count() {
                        stones_next[y0 + y2].push(*x0);
                    }
                }
            }
            for x0 in 0..SIZE {
                for y2 in 0..stones_curr[x0].len() {
                    stones_next[y2].push(x0)
                }
            }
            std::mem::swap(&mut stones_curr, &mut stones_next);
            for x in &mut stones_next {
                x.clear();
            }
            println!("North");
            print(&y_blockers, &stones_curr, true);
            println!();

            // Roll West
            for (x0, line) in x_blockers.iter().enumerate().rev() {
                for y0 in line {
                    for x2 in 1..=stones_curr[*y0].extract_if(|x1| x0 < *x1).count() {
                        stones_next[x0 + x2].push(*y0);
                    }
                }
            }
            for y0 in 0..SIZE {
                for x2 in 0..stones_curr[y0].len() {
                    stones_next[x2].push(y0)
                }
            }
            std::mem::swap(&mut stones_curr, &mut stones_next);
            for x in &mut stones_next {
                x.clear();
            }
            println!("West");
            print(&y_blockers, &stones_curr, false);
            println!();

            // Roll South
            for (y0, line) in y_blockers.iter().enumerate() {
                for x0 in line {
                    for y2 in 1..=stones_curr[*x0].extract_if(|y1| y0 > *y1).count() {
                        stones_next[y0 - y2].push(*x0);
                    }
                }
            }
            for x0 in 0..SIZE {
                for y2 in 1..=stones_curr[x0].len() {
                    stones_next[SIZE - y2].push(x0)
                }
            }
            std::mem::swap(&mut stones_curr, &mut stones_next);
            for x in &mut stones_next {
                x.clear();
            }
            println!("South");
            print(&y_blockers, &stones_curr, true);
            println!();

            // Roll East
            for (x0, line) in x_blockers.iter().enumerate() {
                for y0 in line {
                    for x2 in 1..=stones_curr[*y0].extract_if(|x1| x0 > *x1).count() {
                        stones_next[x0 - x2].push(*y0);
                    }
                }
            }
            for y0 in 0..SIZE {
                for x2 in 1..=stones_curr[y0].len() {
                    stones_next[SIZE - x2].push(y0)
                }
            }
            std::mem::swap(&mut stones_curr, &mut stones_next);
            for x in &mut stones_next {
                x.clear();
            }
            println!("East");
            print(&y_blockers, &stones_curr, false);
            println!();

            let mut s = DefaultHasher::new();
            stones_curr.hash(&mut s);
            let load = calc_weight(&stones_curr, false);
            load.hash(&mut s);
            let state = s.finish();
            println!("{curr_cycle}, {load}");
            if let Some(loop_start) = states.iter().position(|&known_state| known_state == state) {
                Some((loop_start, curr_cycle))
            } else {
                states.push(state);
                loads.push(load);
                None
            }
            // if k == 120 {
            //     Some((92, 99))
            // } else {
            //     None
            // }
        })
        .unwrap();
    let loop_index = (CYCLES - loop_start) % (loop_end - loop_start);
    println!(
        "{loop_start} - {loop_end} | {} {} | {} {}",
        CYCLES - loop_start,
        loop_end - loop_start,
        loop_index,
        loop_index + loop_start
    );

    loads[loop_start + (CYCLES - loop_start) % (loop_end + 1 - loop_start)]
}
