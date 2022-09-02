use std::{fs::read_to_string, num::ParseIntError};

use crate::types::Answer;
use anyhow::Result;

// Rather than simulate the internal timers of flounders individually, I decided
// to track flounders based on their spawning interval.  This approach
// makes spawning a new generation easy and has constant memory complexity.
// Juveniles are tracked seperate from full-grown flounders until the
//
// To avoid this edge case, we track flounders who can't yet spawn seperately
// from those who can.  These 'juveniles' are added to the full-grown flounders
// list once the two-day window has passed and their timer has synced with
// the rest of the flounders.
//
const SPAWN_INTERVAL: usize = 7;
const JUVENILE_PERIOD: usize = 2;

type Flounder = i64;

pub fn solve(path: &str) -> Result<(Answer, Answer)> {
    // Flounders ready to spawn
    let mut flounders: [Flounder; SPAWN_INTERVAL] = read_input(path)?;
    // Flounders that are not ready to spawn.
    let mut juveniles: [Flounder; SPAWN_INTERVAL] = [0; SPAWN_INTERVAL];

    for i in 0..80 {
        spawn(i, &mut flounders, &mut juveniles);
    }
    let soln1 = count_flounders(&flounders, &juveniles);

    for i in 80..256 {
        spawn(i, &mut flounders, &mut juveniles);
    }
    let soln2 = count_flounders(&flounders, &juveniles);

    Ok((Ok(soln1 as i64), Ok(soln2 as i64)))
}

// The original form is a list of the ages of all the currently living flounders.
fn read_input(path: &str) -> Result<[Flounder; SPAWN_INTERVAL]> {
    let original_form: Vec<usize> = read_to_string(path)?
        .split(',')
        .map(str::trim)
        .map(str::parse)
        .collect::<Result<_, ParseIntError>>()?;
    let mut interval_form = [0; SPAWN_INTERVAL];
    for f in original_form {
        interval_form[f] += 1
    }
    Ok(interval_form)
}

fn count_flounders(flounders: &[Flounder], juveniles: &[Flounder]) -> i64 {
    flounders.iter().sum::<i64>() + juveniles.iter().sum::<i64>()
}

fn print_status(
    day: usize,
    flounders: &[Flounder; SPAWN_INTERVAL],
    juveniles: &[Flounder; SPAWN_INTERVAL],
) {
    println!("day {} | {}", day, count_flounders(flounders, juveniles));
    println!("{:?}", flounders);
    println!("{:?}", juveniles);
}

fn spawn(
    day: usize,
    flounders: &mut [Flounder; SPAWN_INTERVAL],
    juveniles: &mut [Flounder; SPAWN_INTERVAL],
) {
    let i = day % SPAWN_INTERVAL;
    juveniles[i] = flounders[i];

    // Adding SPAWN_INTERVAL prevents usize underflow.
    // Yields same result as day - JUVENILE_period mod SPAWN_INTERVAL
    let j = (i + SPAWN_INTERVAL - JUVENILE_PERIOD) % SPAWN_INTERVAL;
    flounders[i] += juveniles[j];
    juveniles[j] = 0;
}
