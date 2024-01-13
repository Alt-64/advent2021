use std::fs::read_to_string;

use indexmap::IndexMap;
use itertools::Itertools;
use nom::AsChar;

fn hash(input: impl Iterator<Item = u8>) -> usize {
    input.fold(0, |acc, cur| 17 * (acc + usize::from(cur)) % 256)
}

pub fn part1() -> usize {
    read_to_string("input/15")
        .unwrap()
        .split([',', '\n'])
        .map(str::bytes)
        .map(hash)
        .sum()
}

pub fn part2() -> usize {
    let input = read_to_string("input/15").unwrap();
    // let input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";
    let steps = input.trim().split([',', '\n']).map(str::bytes);
    // println!("{}", steps.len());
    // let steps = steps.into_iter().map(|x| x.take_while(|b| b.is_ascii_alphabetic())).map(hash).collect::<HashSet<_>>();
    // println!("{}", steps.len());
    let mut boxes = vec![IndexMap::<String, u8>::new(); 256];
    for step in steps {
        let (step1, mut step2) = step.tee();
        let label = step1
            .take_while(|b| b.is_ascii_alphabetic())
            .map(|c| c.as_char())
            .collect::<String>();
        let box_idx = hash(label.bytes());
        match step2.find(|&c| c == b'-' || c == b'=') {
            Some(b'-') => {
                boxes[box_idx].shift_remove(&label);
            },
            Some(b'=') => {
                let focal_len = step2.last().unwrap() - 48;
                boxes[box_idx]
                    .entry(label)
                    .and_modify(|v| *v = focal_len)
                    .or_insert(focal_len);
            },
            Some(_) | None => panic!(),
        }
        // for (i, bocks) in boxes.iter().enumerate() {
        //     print!("Box {i}:");
        //     for (k, v) in bocks {
        //         print!(" [{k} {v}]");
        //     }
        //     println!();
        // }
    }
    let mut power = 0;
    for (i, bocks) in boxes.iter().enumerate() {
        for (j, &v) in bocks.values().enumerate() {
            power += (i + 1) * (j + 1) * usize::from(v)
        }
    }

    power
}
