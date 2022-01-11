use std::{fs::read_to_string, num::ParseIntError};

use crate::types::{Error, Solution};

const SPAWN_INTERVAL: usize = 7;

type Flounder = i64;

pub fn solver(path: &str) -> Result<(Solution, Solution), Error> {
    let mut spawning: [Flounder; SPAWN_INTERVAL] = [0; SPAWN_INTERVAL];
    let mut flounders: [Flounder; SPAWN_INTERVAL] = read_to_string(path)?
        .split(',')
        .map(str::trim)
        .map(str::parse)
        .collect::<Result<Vec<usize>, ParseIntError>>()?
        .into_iter()
        .fold([0; SPAWN_INTERVAL], |mut acc, f| {
            acc[f] += 1;
            return acc;
        });

    for i in 0..80 {
        spawn(i, &mut flounders, &mut spawning);
    }
    let soln1 = count_flounders(&flounders, &spawning);
    for i in 80..256 {
        spawn(i, &mut flounders, &mut spawning);
    }
    let soln2 = count_flounders(&flounders, &spawning);

    Ok((Ok(soln1 as i64), Ok(soln2 as i64)))
}

fn count_flounders(
    flounders: &[Flounder; SPAWN_INTERVAL],
    spawning: &[Flounder; SPAWN_INTERVAL],
) -> Flounder {
    flounders
        .into_iter()
        .zip(spawning)
        .map(|(s, f)| s + f)
        .sum::<Flounder>()
}

fn print_status(
    day: usize,
    flounders: &[Flounder; SPAWN_INTERVAL],
    spawning: &[Flounder; SPAWN_INTERVAL],
) {
    println!(
        "day {} | {}\n{:?}\n{:?}\n",
        day,
        flounders
            .iter()
            .zip(spawning)
            .map(|(s, f)| s + f)
            .sum::<Flounder>(),
        flounders,
        spawning,
    );
}

fn spawn(
    day: usize,
    flounders: &mut [Flounder; SPAWN_INTERVAL],
    spawning: &mut [Flounder; SPAWN_INTERVAL],
) {
    let f = day % 7;
    spawning[f] = flounders[f];

    let e = (day + 5) % 7;
    flounders[f] += spawning[e];
    spawning[e] = 0;
}
