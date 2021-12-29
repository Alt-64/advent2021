mod day1;
mod day2;
mod day3;
mod day4;
mod errors;

type AdventSolver = for<'r> fn(&'r str) -> Result<i32, errors::Error>;
type AdventDay = [AdventSolver; 2];

fn main() {
    let advent: [AdventDay; 4] = [
        [day1::part1, day1::part2],
        [day2::part2, day2::part2],
        [day3::part1, day3::part2],
        [day4::part1, day4::part2],
    ];
    advent
        .iter()
        .enumerate()
        .map(|(mut i, [part1, part2])| {
            i = i + 1;
            let input_path = format!("input_day{}.txt", i);
            let soln1 = part1(input_path.clone().as_str());
            let soln2 = part2(input_path.as_str());
            println!(
                "== Day {} ==\nPart 1: {:?}\nPart 2: {:?}\n",
                i, soln1, soln2
            )
        })
        .for_each(drop);
}
