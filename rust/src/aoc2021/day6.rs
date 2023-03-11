use std::iter::from_fn;
use std::num::ParseIntError;
use std::sync::mpsc::Sender;

use crate::types::BadInputError;
use crate::types::Solution;
use anyhow::Result;

const SPAWN_INTERVAL: usize = 7;
const JUVENILE_PERIOD: usize = 2;

type Fish = usize;

fn solve(input: &str, tx: Sender<(usize, usize, Solution)>) -> anyhow::Result<()> {
    let mature_fish: Vec<Fish> = input
        .split(',')
        .map(str::parse)
        .collect::<Result<Vec<_>, ParseIntError>>()?;
    let mature_fish = to_interval_form(mature_fish)?;
    let juvenile_fish = vec![0; mature_fish.len()];

    let sim = from_fn(|| {
        for mut day in 0.. {
            // Today's mature fish spawn new juveniles
            day %= mature_fish.len();
            juvenile_fish[day] = mature_fish[day];

            // Juvenile fish that spawned JUVENILE_PERIOD days ago (on day x) mature.
            let x = (day + SPAWN_INTERVAL - JUVENILE_PERIOD) % SPAWN_INTERVAL; // Adding SPAWN_INTERVAL prevents underflow.
            mature_fish[day] += juvenile_fish[x];
            juvenile_fish[x] = 0;

            let count = mature_fish.iter().sum::<usize>() + juvenile_fish.iter().sum::<usize>();
            return Some(count);
        }
        None
    });
    let soln_1: usize = sim.take(80).clone().sum();
    tx.send((6, 1, Ok(Box::new(soln_1))));
    let soln_2: usize = sim.take(256 - 80).clone().sum();
    tx.send((6, 2, Ok(Box::new(soln_2))));

    Ok(())
}

fn to_interval_form(original_form: Vec<Fish>) -> Result<Vec<Fish>, BadInputError> {
    // The original form is a growable list of each fish's age.
    // Interval form is a constant size list and tracks how many fish spawned on each day.
    // e.g.
    //     original form: [0, 0, 1, 1, 1, 2, 5]
    //     interval form: [2, 4, 1, 0]

    let interval_len = original_form.iter().max().ok_or(BadInputError)?;
    let mut interval_form = vec![0; interval_len + 1];
    for f in original_form {
        interval_form[f] += 1;
    }

    Ok(interval_form)
}
