use std::fs::read_to_string;

use itertools::Itertools;

#[derive(Clone, Copy, Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn next_direction(pipe: char, direction: Direction) -> Direction {
    match (pipe, direction) {
        ('-', f) | ('|', f) => f,
        ('F', Direction::Right) | ('7', Direction::Left) => Direction::Up,
        ('F', Direction::Down) | ('L', Direction::Up) => Direction::Left,
        ('J', Direction::Up) | ('7', Direction::Down) => Direction::Right,
        ('J', Direction::Left) | ('L', Direction::Right) => Direction::Down,
        _ => panic!(),
    }
}

fn next_coord((x, y): (usize, usize), direction: Direction) -> (usize, usize) {
    match direction {
        Direction::Up => (x, y + 1),
        Direction::Down => (x, y - 1),
        Direction::Left => (x + 1, y),
        Direction::Right => (x - 1, y),
    }
}

type Coordinate = (usize, usize);

fn next(
    pipemap: &Vec<Vec<char>>,
    (x, y): Coordinate,
    direction: Direction,
) -> (Coordinate, Direction) {
    let (next_x, next_y) = next_coord((x, y), direction);
    return (
        (next_x, next_y),
        next_direction(pipemap[next_y][next_x], direction),
    );
}

pub fn part1() -> usize {
    let input = read_to_string("input/10").unwrap();
    let pipemap = input.lines().map(|x| x.chars().collect_vec()).collect_vec();
    let (start_x, start_y) = (28, 109);

    let (mut ax, mut ay) = (start_x + 1, start_y);
    let mut a_direction = Direction::Down;

    let (mut bx, mut by) = (start_x, start_y + 1);
    let mut b_direction = Direction::Up;

    let mut steps: usize = 1;

    while ax != bx || ay != by {
        ((ax, ay), a_direction) = next(&pipemap, (ax, ay), a_direction);
        ((bx, by), b_direction) = next(&pipemap, (bx, by), b_direction);
        steps += 1
    }

    return steps;
}

fn better_chars(input: &char) -> char {
    match input {
        'F' => '┌',
        'J' => '┘',
        'L' => '└',
        '7' => '┐',
        '-' => '─',
        '│' => '|',
         x => *x,
    }
}

pub fn part2() -> i64 {
    let input = read_to_string("input/10").unwrap();
    let pipemap = input.lines().map(|x| x.chars().collect_vec()).collect_vec();
    let mut loopmap = vec![vec![' '; 140]; 140];
    let (start_x, start_y) = (28, 109);

    loopmap[start_y][start_x] = 'F';
    let (mut ax, mut ay) = (start_x + 1, start_y);
    let mut a_direction = Direction::Down;
    loopmap[ay][ax] = pipemap[ay][ax];

    let (mut bx, mut by) = (start_x, start_y + 1);
    let mut b_direction = Direction::Up;
    loopmap[by][bx] = pipemap[by][bx];

    while ax != bx || ay != by {
        ((ax, ay), a_direction) = next(&pipemap, (ax, ay), a_direction);
        ((bx, by), b_direction) = next(&pipemap, (bx, by), b_direction);
        loopmap[ay][ax] = pipemap[ay][ax];
        loopmap[by][bx] = pipemap[by][bx];
    }

    let mut inside_tile_count = 0;
    for row in loopmap.iter_mut() {
        let mut inside = false;
        let mut onpipe = ' ';
        for tile in row.iter_mut() {
            let x = inside_tile_count;
            match tile {
                ' ' if inside && onpipe == ' ' => inside_tile_count += 1,
                '|' => inside = !inside,
                'F' | 'L' => onpipe = *tile,
                'J' => {
                    inside = inside != (onpipe == 'F');
                    onpipe = ' '
                },
                '7' => {
                    inside = inside != (onpipe == 'L');
                    onpipe = ' '
                },
                _ => ()
            }
            if x != inside_tile_count {
                *tile = '*'
            }
        }
        println!("{}", String::from_iter(row.iter().map(better_chars)));
    }

    inside_tile_count
}
