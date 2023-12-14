mod day1;
mod day2;
mod day3;
mod day4;
mod day5;

// macro_rules! test_day {
//     ( $day:path, $x: literal, $a:literal, $b:literal ) => {
//         #[test]
//         fn day() {
//             let input = read_to_string(format!("input/input-{}", $x)).unwrap();
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
        let input = read_to_string("input/input-1").unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 54331);
        let soln2 = _part2(&input);
        assert_eq!(soln2, 54518);
    }

    #[test]
    fn day2() {
        use crate::day2::*;
        let input = read_to_string("input/input-2").unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 2285);
        let soln2 = _part2(&input);
        assert_eq!(soln2, 77021);
    }

    #[test]
    fn day3() {
        use crate::day3::*;
        let input = read_to_string("input/input-3").unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 532331);
        let soln2 = _part2(&input);
        assert_eq!(soln2, 82301120);
    }

    #[test]
    fn day4() {
        use crate::day4::*;
        let input = read_to_string("input/input-4").unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 23235);
        let soln2 = _part2(&input);
        assert_eq!(soln2, 5920640);
    }

    #[test]
    fn day5() {
        use crate::day5::*;
        let input = read_to_string("input/input-5").unwrap();
        let input = input.trim().to_string();

        let soln1 = _part1(&input);
        assert_eq!(soln1, 175622908);
        let soln2 = _part2(&input);
        assert_eq!(soln2, 0);
    }
}
