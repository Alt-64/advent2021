use std::fmt::Debug;
use std::iter::from_fn;
use std::iter::FromFn;
use std::num::ParseIntError;

use crate::types::{BadInputError, SolveState, Solver};
use anyhow::Result;

const SPAWN_INTERVAL: usize = 7;
const JUVENILE_PERIOD: usize = 2;

type Fish = usize;

pub struct Day6 {
    state: SolveState,
    sim: FromFn<Box<dyn FnMut() -> Option<usize>>>,
}

impl Solver<'_> for Day6 {
    type Soln1 = usize;
    fn solve_part1(&mut self) -> Self::Soln1 {
        self.sim.take(80).clone().sum()
    }

    type Soln2 = usize;
    fn solve_part2(&mut self) -> Self::Soln2 {
        self.sim.take(256 - 80).sum()
    }
}

impl Day6 {
    fn new(mut mature_fish: Vec<Fish>, mut juvenile_fish: Vec<Fish>) -> Self {
        let sim = from_fn(move || {
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
        Day6 {
            state: SolveState::new(),
            sim: Box::new(sim),
        }
    }
}

impl TryFrom<&str> for Day6 {
    type Error = anyhow::Error;

    fn try_from(input: &str) -> std::result::Result<Self, Self::Error> {
        let mature_fish: Vec<Fish> = input
            .split(',')
            .map(str::parse)
            .collect::<Result<Vec<_>, ParseIntError>>()?;
        let mature_fish = to_interval_form(mature_fish)?;
        let juvenile_fish = vec![0; mature_fish.len()];

        Ok(Day6::new(mature_fish, juvenile_fish))
    }
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

impl Iterator for Day6 {
    type Item = Box<dyn Debug>;

    fn next(&mut self) -> Option<Self::Item> {
        self.state.next()
    }
}
