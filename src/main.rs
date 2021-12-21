mod day1;
mod day2;
mod day3;
mod errors;

type AdventSolver = for<'r> fn(&'r str) -> Result<i32, errors::Error>;
type AdventDay = [AdventSolver; 2];

fn main() {
    let advent: [AdventDay; 3] = [
        [day1::part1, day1::part2],
        [day2::part2, day2::part2],
        [day3::part1, day3::part2],
    ];
    let shutup_linter = advent
        .iter()
        .enumerate()
        .map(|(i, [part1, part2])| {
            let input_path = format!("input_day{}.txt", i + 1);
            let soln1 = part1(input_path.clone().as_str());
            let soln2 = part2(input_path.as_str());
            println!(
                "== Day {} ==\nPart 1: {:?}\nPart 2: {:?}\n",
                i + 1,
                soln1,
                soln2
            )
        })
        .collect::<Vec<_>>();
}
