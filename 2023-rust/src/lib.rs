#![allow(dead_code)]

mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;
mod day10;

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
    fn day1() {
        use crate::day1::*;
        let input = read_to_string("input/1").unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 54331);
        let soln2 = _part2(&input);
        assert_eq!(soln2, 54518);
    }

    #[test]
    fn day2() {
        use crate::day2::*;
        let input = read_to_string("input/2").ok().unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 2285);
        let soln2 = _part2(&input);
        assert_eq!(soln2, 77021);
    }

    #[test]
    fn day3() {
        use crate::day3::*;
        let input = read_to_string("input/3").unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 532331);
        let soln2 = _part2(&input);
        assert_eq!(soln2, 82301120);
    }

    #[test]
    fn day4() {
        use crate::day4::*;
        let input = read_to_string("input/4").unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 23235);
        let soln2 = _part2(&input);
        assert_eq!(soln2, 5920640);
    }

    #[test]
    fn day5() {
        use crate::day5::*;
        let input = read_to_string("input/5").unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 175622908);
        let soln2 = _part2(&input);
        assert_eq!(soln2, 5200543);
    }

    #[test]
    fn day6() {
        use crate::day6::*;
        let input = read_to_string("input/6").unwrap();
        let input = input.trim().to_string();
        assert_eq!(_part1(&input), 220320);
        assert_eq!(_part2(&input), 34454850);
    }

    #[test]
    fn day7() {
        use crate::day7::*;
        assert_eq!(_part1(), 250946742);
        assert_eq!(_part2(), 251824095);
    }

    #[test]
    fn day8() {
        use crate::day8::*;
        assert_eq!(_part1(), 16043);
        assert_eq!(_part2(), 15726453850399);
    }

    #[test]
    fn day9() {
        use crate::day9::*;
        assert_eq!(part1(), 1861775706);
        assert_eq!(part2(), 1082);
    }

    #[test]
    fn day10() {
        use crate::day10::*;
        assert_eq!(part1(), 6931);
        assert_eq!(part2(), 0);
    }
}
