use std::{collections::HashMap, fs::read_to_string, ops::AddAssign};

use itertools::Itertools;
use nom::{
    character::complete::{alphanumeric0, digit1, newline, space1},
    multi::separated_list1,
    sequence::separated_pair,
    IResult,
};

fn read_input(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
    separated_list1(newline, separated_pair(alphanumeric0, space1, digit1))(input)
}

fn map_cards(c: char) -> usize {
    match c {
        '2' => 0,
        '3' => 1,
        '4' => 2,
        '5' => 3,
        '6' => 4,
        '7' => 5,
        '8' => 6,
        '9' => 7,
        'T' => 8,
        'J' => 9,
        'Q' => 10,
        'K' => 11,
        'A' => 12,
        _ => panic!(),
    }
}

fn score(input: &str) -> usize {
    let mut tallies = HashMap::new();
    let mut cards: Vec<_> = input.chars().map(map_cards).collect();
    for card in &cards {
        tallies
            .entry(card)
            .and_modify(|x: &mut i32| x.add_assign(1))
            .or_insert(1);
    }
    let x = match tallies.len() {
        1 => 6,
        2 if tallies.values().find(|x| **x == 4).is_some() => 5,
        2 => 4,
        3 if tallies.values().find(|x| **x == 3).is_some() => 3,
        3 => 2,
        4 => 1,
        5 => 0,
        _ => panic!(),
    };
    cards.insert(0, x);
    cards
        .iter()
        .rev()
        .enumerate()
        .fold(0, |acc, (i, cur)| acc + 16usize.pow(i as u32) * cur)
}

fn map_cards2(c: char) -> usize {
    match c {
        'J' => 0,
        '2' => 1,
        '3' => 2,
        '4' => 3,
        '5' => 4,
        '6' => 5,
        '7' => 6,
        '8' => 7,
        '9' => 8,
        'T' => 9,
        'Q' => 10,
        'K' => 11,
        'A' => 12,
        _ => panic!(),
    }
}

#[derive(Copy, Clone, Debug)]
enum HandType {
    HighCard,
    OnePair,
    TwoPair,
    ThreeKind,
    FullHouse,
    FourKind,
    FiveKind,
}

fn score2(input: &str) -> usize {
    let mut tallies = HashMap::new();
    let mut cards: Vec<_> = input.chars().map(map_cards2).collect();
    for card in &cards {
        tallies
            .entry(card)
            .and_modify(|x: &mut i32| x.add_assign(1))
            .or_insert(1);
    }
    let mut hand_type: HandType = match tallies.len() {
        1 => HandType::FiveKind,
        2 if tallies.values().find(|x| **x == 4).is_some() => HandType::FourKind,
        2 => HandType::FullHouse,
        3 if tallies.values().find(|x| **x == 3).is_some() => HandType::ThreeKind,
        3 => HandType::TwoPair,
        4 => HandType::OnePair,
        5 => HandType::HighCard,
        _ => panic!(),
    };
    if let Some(jokers) = tallies.get(&0) {
        hand_type = match (jokers, hand_type) {
            (1, HandType::FiveKind) => HandType::FiveKind,
            (1, HandType::FourKind) => HandType::FiveKind,
            (1, HandType::ThreeKind) => HandType::FourKind,
            (1, HandType::TwoPair) => HandType::FullHouse,
            (1, HandType::OnePair) => HandType::ThreeKind,
            (1, HandType::HighCard) => HandType::OnePair,

            (2, HandType::FullHouse) => HandType::FiveKind,
            (2, HandType::TwoPair) => HandType::FourKind,
            (2, HandType::OnePair) => HandType::ThreeKind,

            (3, HandType::FullHouse) => HandType::FiveKind,
            (3, HandType::ThreeKind) => HandType::FourKind,

            (4, HandType::FourKind) => HandType::FiveKind,

            (5, HandType::FiveKind) => HandType::FiveKind,
            _ => panic!(),
        }
    }
    cards.insert(0, hand_type as usize);
    let x = cards
        .iter()
        .rev()
        .enumerate()
        .fold(0, |acc, (i, cur)| acc + 16usize.pow(i as u32) * cur);
    x
}

pub fn _part1() -> usize {
    let input = read_to_string("input/7").unwrap().trim().to_string();
    // let input = read_to_string("input/6-test").unwrap();

    let (_leftovers, hands) = read_input(&input).unwrap();
    hands
        .into_iter()
        .map(|(hand, bid)| (score(hand), bid))
        .sorted_by(|(hand1, _), (hand2, _)| hand1.cmp(hand2))
        .enumerate()
        .map(|(rank, (score, bid))| (rank + 1) * bid.parse::<usize>().unwrap())
        .sum()
}

pub fn _part2() -> usize {
    let input = read_to_string("input/7").unwrap().trim().to_string();
    // let input = read_to_string("input/6-test").unwrap();

    let (_leftovers, hands) = read_input(&input).unwrap();
    hands
        .into_iter()
        .map(|(hand, bid)| (score2(hand), bid))
        .sorted_by(|(hand1, _), (hand2, _)| hand1.cmp(hand2))
        .enumerate()
        .map(|(rank, (score, bid))| (rank + 1) * bid.parse::<usize>().unwrap())
        .sum()
}
