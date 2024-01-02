#![allow(dead_code)]

mod day01;
mod day10;
mod day11;
mod day12;
mod day13;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;

// macro_rules! test_day {
//     ( $day:path, $x: literal, $a:literal, $b:literal ) => {
//         #[test]
//         fn day() {
//             let input = read_to_string(format!("input/{}", $x)).unwrap();
//             let input = input.trim().to_string();

//             let soln1 = $day::*::_part1(&input);
//             assert_eq!(soln1, $a);
//             let soln2 = _part2(&input);
//             assert_eq!(soln2, $b);
//         }
//     }
// }

#[cfg(test)]
mod tests {
    use std::fs::read_to_string;

    #[test]
    fn day01() {
        use crate::day01::*;
        let input = read_to_string("input/1").unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 54331);
        let soln2 = _part2(&input);
        assert_eq!(soln2, 54518);
    }

    #[test]
    fn day02() {
        use crate::day02::*;
        let input = read_to_string("input/2").ok().unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 2285);
        let soln2 = _part2(&input);
        assert_eq!(soln2, 77021);
    }

    #[test]
    fn day03() {
        use crate::day03::*;
        let input = read_to_string("input/3").unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 532331);
        let soln2 = _part2(&input);
        assert_eq!(soln2, 82301120);
    }

    #[test]
    fn day04() {
        use crate::day04::*;
        let input = read_to_string("input/4").unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 23235);
        let soln2 = _part2(&input);
        assert_eq!(soln2, 5920640);
    }

    #[test]
    fn day05() {
        use crate::day05::*;
        let input = read_to_string("input/5").unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 175622908);
        let soln2 = _part2(&input);
        assert_eq!(soln2, 5200543);
    }

    #[test]
    fn day06() {
        use crate::day06::*;
        let input = read_to_string("input/6").unwrap();
        let input = input.trim().to_string();
        assert_eq!(_part1(&input), 220320);
        assert_eq!(_part2(&input), 34454850);
    }

    #[test]
    fn day07() {
        use crate::day07::*;
        assert_eq!(_part1(), 250946742);
        assert_eq!(_part2(), 251824095);
    }

    #[test]
    fn day08() {
        use crate::day08::*;
        assert_eq!(_part1(), 16043);
        assert_eq!(_part2(), 15726453850399);
    }

    #[test]
    fn day09() {
        use crate::day09::*;
        assert_eq!(part1(), 1861775706);
        assert_eq!(part2(), 1082);
    }

    #[test]
    fn day10() {
        use crate::day10::*;
        assert_eq!(part1(), 6931);
        assert_eq!(part2(), 357);
    }

    #[test]
    fn day11() {
        use crate::day11::*;
        assert_eq!(part1(), 9543156);
        assert_eq!(part2(), 625243292686);
    }

    #[test]
    fn day12() {
        use crate::day12::*;
        assert_eq!(part1(), 7674);
        assert_eq!(part2(), 4443895258186);
    }
}
