use std::{convert::TryFrom, num::ParseIntError};

use crate::types::Answer;
use anyhow::Result;

const SPAWN_INTERVAL: usize = 7;
const JUVENILE_PERIOD: usize = 2;

type Flounder = i64;

pub fn solve(input: &str) -> Result<(Answer, Answer)> {
    let mut flounders = Flounders::<SPAWN_INTERVAL>::try_from(input)?;

    for i in 0..80 {
        flounders.spawn(i)
    }
    let soln1 = Ok(flounders.count());

    for i in 80..256 {
        flounders.spawn(i)
    }
    let soln2 = Ok(flounders.count());

    Ok((soln1, soln2))
}

struct Flounders<const I: usize> {
    // Flounders that can spawn.
    mature: [Flounder; I],
    // Flounders that can not yet spawn.
    juvenile: [Flounder; I],
}

impl<const I: usize> TryFrom<&str> for Flounders<I> {
    type Error = ParseIntError;

    fn try_from(input: &str) -> Result<Flounders<I>, Self::Error> {
        // The original form is a growable list of of all the flounders' ages.
        // i.e. [0, 0, 1, 1, 1, 2, 5]
        let original_form: Vec<usize> = input
            .split(',')
            .map(str::parse)
            .collect::<Result<_, ParseIntError>>()?;

        // Interval form is constant size and tracks flounders by what day they spawn on.
        // i.e. [2, 4, 1, 0] where the spawn interval is 4
        let mut interval_form = [0; I];
        for f in original_form {
            interval_form[f] += 1
        }

        Ok(Flounders {
            mature: interval_form,
            juvenile: [0; I],
        })
    }
}

impl<const I: usize> Flounders<I> {
    fn spawn(&mut self, day: usize) {
        let i = day % SPAWN_INTERVAL;
        self.juvenile[i] = self.mature[i];

        // Adding SPAWN_INTERVAL prevents usize underflow.
        let j = (i + SPAWN_INTERVAL - JUVENILE_PERIOD) % SPAWN_INTERVAL;
        self.mature[i] += self.juvenile[j];
        self.juvenile[j] = 0;
    }

    fn count(&self) -> i64 {
        self.mature.iter().sum::<i64>() + self.juvenile.iter().sum::<i64>()
    }
}
