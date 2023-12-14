use regex::Regex;

pub fn _part1(input: &str) -> u32 {
    input.split('\n').map(|line| {
        let first = line.chars().find(char::is_ascii_digit).unwrap() as u8 - 48;
        let last = line.chars().rev().find(char::is_ascii_digit).unwrap() as u8 - 48;
        (10*first + last) as u32
    }).sum()
}

pub fn _part2(input: &str) -> u32 {
    let re_left = Regex::new(r"(one)|(two)|(three)|(four)|(five)|(six)|(seven)|(eight)|(nine)|(\d)").unwrap();
    let re_right = Regex::new(r"(eno)|(owt)|(eerht)|(ruof)|(evif)|(xis)|(neves)|(thgie)|(enin)|(\d)").unwrap();

    input.split('\n').map(|line| {
        let first = match re_left.captures(line).unwrap().get(0).unwrap().as_str() {
                "one" => '1',
                "two" => '2',
                "three" => '3',
                "four" => '4',
                "five" => '5', 
                "six" => '6',
                "seven" => '7',
                "eight" => '8',
                "nine" => '9',
                x => x.chars().next().unwrap(),
        } as u8 - 48;

        let reversed = line.chars().rev().collect::<String>();
        let last = match re_right.captures(&reversed).unwrap().get(0).unwrap().as_str() { 
                "eno" => '1',
                "owt" => '2',
                "eerht" => '3',
                "ruof" => '4',
                "evif" => '5', 
                "xis" => '6',
                "neves" => '7',
                "thgie" => '8',
                "enin" => '9',
                x => x.chars().next().unwrap(),
        } as u8 - 48;

        (10*first + last) as u32
    }).sum()
}
