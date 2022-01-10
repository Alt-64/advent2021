use std::fs::read_to_string;

use crate::types::{Error, Solution};

const SPAWN_INTERVAL: usize = 7;

type Flounder = u128;

pub fn solver(path: &str) -> Result<(Solution, Solution), Error> {
    let mut gen_a: [Flounder; SPAWN_INTERVAL] = read_to_string(path)?
        .split(',')
        .map(|timer| Ok(timer.trim().parse::<usize>()?))
        .collect::<Result<Vec<usize>, Error>>()?
        .into_iter()
        .fold([0; SPAWN_INTERVAL], |mut acc, f| {
            acc[f] += 1;
            return acc;
        });

    let mut soln1 = 0;
    let generations = 256 / SPAWN_INTERVAL;
    let remainder = 256 % SPAWN_INTERVAL;
    println!("{}:{}", generations, remainder);
    // for i in 0..generations {
    //     println!("{:?} - {}", flounders, flounders.iter().sum::<u128>());
    //     flounders = spawn_children(&flounders);
    //     if i == 80 {
    //         soln1 = flounders.iter().sum::<u128>() as i32;
    //     }
    // }

    println!("{:?} - {}", gen_a, gen_a.iter().sum::<u128>());
    let gen_b = [0; SPAWN_INTERVAL];
    let gen_c = spawn_children(&gen_a, 7);
    for i in 2..SPAWN_INTERVAL {
        gen_a[i] += gen_b[i];
    }
    for i in 2..SPAWN_INTERVAL {
        gen_a[i] += gen_c[i];
    }

    println!("{:?} - {}", gen_a, gen_a.iter().sum::<u128>());
    let gen_b = gen_c;
    let gen_c = spawn_children(&gen_a, 7);
    for i in 0..2 {
        gen_a[i] += gen_b[i];
    }
    for i in 2..SPAWN_INTERVAL {
        gen_a[i] += gen_c[i];
    }
    println!("{:?} - {}", gen_a, gen_a.iter().sum::<u128>());
    let gen_b = gen_c;
    let gen_c = spawn_children(&gen_a, 4);
    for i in 0..2 {
        gen_a[i] += gen_b[i];
    }
    for i in 2..SPAWN_INTERVAL {
        gen_a[i] += gen_c[i];
    }
    println!("{:?} - {}", gen_a, gen_a.iter().sum::<u128>());
    let soln2 = gen_a.iter().sum::<u128>();
    println!("{}", soln2);

    Ok((Ok(soln1), Ok(soln2 as i32)))
}

fn spawn_children(
    flounders: &[Flounder; SPAWN_INTERVAL],
    days: usize,
) -> [Flounder; SPAWN_INTERVAL] {
    let mut new_flounders = [0; SPAWN_INTERVAL];
    for (i, f) in flounders.iter().enumerate().take(days) {
        let j = (i + 2) % SPAWN_INTERVAL;
        new_flounders[j] = *f;
        println!(
            "day {} - {:?} - {}",
            i,
            new_flounders,
            new_flounders.iter().sum::<u128>()
        );
    }
    return new_flounders;
}
