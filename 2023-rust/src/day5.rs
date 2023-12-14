use std::cmp::{min, max};

use itertools::Itertools;
use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, digit1, line_ending, newline},
    combinator::{map, map_opt, map_res},
    multi::separated_list1,
    sequence::{delimited, pair},
    IResult,
};

fn read_seeds(input: &[u8]) -> IResult<&[u8], Vec<u64>> {
    delimited(
        tag("seeds: "),
        separated_list1(
            nom::character::complete::char(' '),
            map(digit1, read_number),
        ),
        tag("\n\n"),
    )(input)
}

fn read_number(input: &[u8]) -> u64 {
    String::from_utf8(input.to_vec()).unwrap().parse().unwrap()
}

fn read_map(input: &[u8]) -> IResult<&[u8], Vec<(u64, u64, u64)>> {
    separated_list1(
        line_ending,
        map_opt(
            separated_list1(nom::character::complete::char(' '), digit1),
            |x| x.into_iter().map(read_number).collect_tuple().map(|(a,b,c)| (a,b,b+c))
        ),
    )(input)
}

fn read_maps(input: &[u8]) -> IResult<&[u8], Vec<Vec<(u64, u64, u64)>>> {
    separated_list1(pair(line_ending, line_ending), read_map)(input)
}

// fn read_map_name(input: &[u8]) -> IResult<&[u8], Vec<&[u8]>> {
//     delimited(
//         tag("\n\n"),
//         separated_list1(nom::character::complete::char('-'), alpha1),
//         tag(" map:\n"),
//     )(input)
// }

pub fn _part1(input: &str) -> u64 {
    let (_leftover, (seeds, maps)) = pair(read_seeds, read_maps)(input.as_bytes()).unwrap();

    let mut smallest = u64::MAX;
    for seed in seeds {
        let mut x = seed;
        for m in &maps {
            for (d, left, right) in m {
                if *left <= x && x <= *right {
                    let y = d + (x - left);
                    println!("{x} -> {y}");
                    x = y;
                    break;
                }
            }
        }
        smallest = min(smallest, x);
    }

    smallest

    // let seeds = read_seeds(lines.next().unwrap());
}

fn merge(mut intervals: Vec<(u64, u64)>) -> Vec<(u64, u64)> {
    println!("{intervals:?}");
    let mut curr_stack = Vec::new();
    let mut intervals = intervals.iter();
    curr_stack.push(intervals.next().unwrap().clone());
    for (left_0, right_0) in intervals {
        let (left_1, right_1) = curr_stack.last_mut().unwrap();
        
        print!("{:#03?},{:#03?} and {:#03?},{:#03?}", left_0, right_0, left_1, right_1);
        if left_0 <= right_1 && right_0 >= left_1 {
            print!("merged {:#03?},{:#03?} and {:#03?},{:#03?}", left_0, right_0, left_1, right_1);
            *left_1 = *min(left_0, left_1);
            *right_1 = *max(right_0, right_1);
            println!(" into {:#03?},{:#03?}", left_0, right_0);
        } else {
            curr_stack.push((*left_0, *right_0))
        }
    }
    curr_stack
}

const SIZE: u64 = 1 << 16;

pub fn _part2(input: &str) -> u64 {
    println!("WHAT THE FUCK");
    let (_leftover, (seeds, maps)) = pair(read_seeds, read_maps)(input.as_bytes()).unwrap();
    let seeds: Vec<(u64, u64)> = seeds
        .into_iter()
        // .map(|x| x % SIZE)
        .chunks(2)
        .into_iter()
        .map(|x| x.collect_tuple())
        // .take(5)
        .flatten()
        .map(|(left, offset)| (left, left+offset - 1))
        .collect();

    let mut curr_stack = seeds;
    let mut next_stack = Vec::new();

    // for map in &maps.into_iter().map(|v| v.into_iter().map(|(x, y, z)| (x % SIZE, y % SIZE, z % SIZE)).collect_vec()).collect_vec() {
    for map in &maps {
        curr_stack = merge(curr_stack);
        println!("{:?}", curr_stack);
        while let Some((left_0, right_0)) = curr_stack.pop() {
            let mut mapped = false;
            for &(dest, left_1, right_1) in map {
                if left_0 <= right_1 && right_0 >= left_1 {
                    mapped = true;

                    let left_overlap = max(left_0, left_1);
                    let right_overlap = min(right_0, right_1);

                    next_stack.push((dest + left_overlap - left_0, dest + right_overlap - right_0));

                    if left_0 < left_overlap {
                        curr_stack.push((left_0, left_overlap));
                    }

                    if right_overlap < right_0 {
                        curr_stack.push((right_overlap, right_0));
                    }

                    break;
                }
            }
            if !mapped {
                next_stack.push((left_0, right_0))
            }
        }
        // Swap Stacks
        let tmp = curr_stack;
        curr_stack = next_stack;
        next_stack = tmp;
    }
    // curr_stack = merge(curr_stack);
    println!("{:010?}", curr_stack);
    curr_stack.into_iter().map(|(left, _right)| left).min().unwrap()

}
