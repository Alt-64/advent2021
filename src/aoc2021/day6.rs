use std::fs::read_to_string;

use num::Float;

use crate::types::{Error, Solution};

const SPAWN_INTERVAL: usize = 7;
const SPAWN_TIMER: i32 = SPAWN_INTERVAL as i32 + 2;

type Flounder = i32;

pub fn solver(path: &str) -> Result<(Solution, Solution), Error> {
    let flounders: Vec<Flounder> = read_to_string(path)?
        .split(',')
        .map(|timer| Ok(-SPAWN_TIMER + timer.trim().parse::<i32>()?))
        .collect::<Result<Vec<Flounder>, Error>>()?;

    let soln1 = count_flounders(&flounders, 80) as i32;
    let soln2 = count_flounders(&flounders, 256) as i32;

    Ok((Ok(soln1), Ok(soln2)))
}

fn count_flounders(flounders: &Vec<Flounder>, last_day: i32) -> usize {
    let mut flounders = flounders.clone();
    let mut count = flounders.len();
    while let Some(new_flounders) = spawn_generation(&flounders, last_day) {
        count += new_flounders.len() as usize;
        flounders = new_flounders;
    }
    return count;
}

fn spawn_generation(flounders: &Vec<Flounder>, last_day: i32) -> Option<Vec<Flounder>> {
    if flounders.len() > 0 {
        let children = flounders
            .into_iter()
            .flat_map(|spawn_day| spawn_children(spawn_day, last_day))
            .collect();
        Some(children)
    } else {
        None
    }
}

fn spawn_children(spawn_day: &i32, last_day: i32) -> Vec<i32> {
    (spawn_day + SPAWN_TIMER..last_day)
        .step_by(SPAWN_INTERVAL)
        .collect::<Vec<i32>>()
}
