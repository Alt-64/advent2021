use std::{iter::from_fn, num::ParseIntError};

use crate::types::{Answer, NoSolutionError};
use anyhow::Result;
use itertools::Itertools;

const SPAWN_INTERVAL: usize = 7;
const JUVENILE_PERIOD: usize = 2;

type Fish = i64;

pub fn solve(input: &str) -> Result<(Answer, Answer)> {
    let mature_fish = get_fish(input)?;
    let juvenile_fish: Vec<Fish> = vec![0; mature_fish.len()];

    let (fish_sim_1, fish_sim_2) = simulate_fish(mature_fish, juvenile_fish).take(80).tee();
    let soln1 = fish_sim_1.sum();
    let soln2 = fish_sim_2.take(256 - 80).sum::<i64>() + soln1;

    Ok((Ok(soln1), Ok(soln2)))
}

fn get_fish(input: &str) -> Result<Vec<Fish>> {
    // The original form is a growable list of each fish's age.
    // Interval form is a constant size list and tracks how many fish spawned on each day.
    // e.g.
    //     original form: [0, 0, 1, 1, 1, 2, 5]
    //     interval form: [2, 4, 1, 0]
    let original_form: Vec<usize> = input
        .split(',')
        .map(str::parse)
        .collect::<Result<_, ParseIntError>>()?;

    let interval_len = original_form.iter().max().ok_or(NoSolutionError)?;
    let mut interval_form = vec![0; interval_len + 1];
    for f in original_form {
        interval_form[f] += 1
    }

    Ok(interval_form)
}

fn simulate_fish(
    mut mature_fish: Vec<Fish>,
    mut juvenile_fish: Vec<Fish>,
) -> impl Iterator<Item = i64> {
    from_fn(move || {
        for mut day in 0.. {
            // Today's mature fish spawn new juveniles
            day %= mature_fish.len();
            juvenile_fish[day] = mature_fish[day];

            // Fish that spawned JUVENILE_PERIOD days ago mature
            let j = (day + SPAWN_INTERVAL - JUVENILE_PERIOD) % SPAWN_INTERVAL; // Adding SPAWN_INTERVAL prevents underflow.
            mature_fish[day] += juvenile_fish[j];
            juvenile_fish[j] = 0;

            let count = mature_fish.iter().sum::<i64>() + juvenile_fish.iter().sum::<i64>();
            return Some(count);
        }
        None
    })
}
