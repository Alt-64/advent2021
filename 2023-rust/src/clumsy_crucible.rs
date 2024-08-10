use std::{
    cmp::{self, min, Ordering, Reverse},
    collections::HashMap,
    fs::read_to_string,
    hash::Hash,
};

use priority_queue::PriorityQueue;

fn read_grid(input: &str) -> ((usize, usize), HashMap<(usize, usize), u32>) {
    let grid = input
        .lines()
        .enumerate()
        .map(|(i, row)| {
            row.chars().enumerate().flat_map(move |(j, c)| {
                let x = c.to_digit(10)?;
                Some(((j, i), x))
            })
        })
        .flatten()
        .collect::<HashMap<_, _>>();

    let maxs = grid.keys().fold((0, 0), |(x_max, y_max), (x, y)| {
        (cmp::max(x_max, *x), cmp::max(y_max, *y))
    });

    (maxs, grid)
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash, Ord)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn next_directions(&self) -> [Self; 2] {
        match self {
            Direction::Up => [Direction::Left, Direction::Right],
            Direction::Down => [Direction::Right, Direction::Left],
            Direction::Right => [Direction::Up, Direction::Down],
            Direction::Left => [Direction::Down, Direction::Up],
        }
    }
}

impl PartialOrd for Direction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Direction::Right | Direction::Up, Direction::Left | Direction::Down) => {
                Some(Ordering::Greater)
            }
            (Direction::Left | Direction::Down, Direction::Right | Direction::Up) => {
                Some(Ordering::Less)
            }
            (_, _) => Some(Ordering::Equal),
        }
    }
}

const EXAMPLE: &str = "\
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533";

#[derive(Debug, Hash, Ord, Clone, Copy, PartialEq, Eq, PartialOrd)]
struct Crucible {
    x: usize,
    y: usize,
    heat_lost: Reverse<u32>,
    direction: Direction,
}

impl Crucible {
    fn new(x: usize, y: usize, direction: Direction, heat_lost: u32) -> Self {
        Self {
            x,
            y,
            direction,
            heat_lost: Reverse(heat_lost),
        }
    }
}

// struct PriorityQueue<P: Eq + Hash, V>(BTreeMap<P, V>);

// impl<P: Eq + Hash, V> PriorityQueue<P, V> {
//     fn new() -> Self {
//         PriorityQueue(HashMap::new())
//     }

//     fn push(&mut self, priority: P, value: V) {
//         self.0.insert(priority, value);
//     }

//     fn pop(&mut self) -> (P, V) {}
// }
//
//
//
#[derive(Copy, Clone)]
enum Side {
    Left,
    Right,
}
impl Side {
    pub fn curry<T: Copy>(self, outer: T) -> impl Fn(T) -> (T, T) {
        move |inner| match self {
            Side::Left => (outer, inner),
            Side::Right => (inner, outer),
        }
    }
}

fn min_heat(input: &str, min_move: usize, max_move: usize) -> usize {
    let ((x_max, y_max), grid) = read_grid(&input);

    let mut pq = PriorityQueue::new();
    let a = Crucible::new(0, 0, Direction::Right, 0);
    let b = Crucible::new(0, 0, Direction::Up, 0);
    pq.push(a, a);
    pq.push(b, b);

    let mut memo = HashMap::<(usize, usize), u32>::new();

    let mut i = 0;
    let mut total_min_heat_lost = u32::MAX;
    while let Some((_, curr)) = pq.pop() {
        dbg!(curr);
        i += 1;
        if i > 1000000 {
            panic!();
        }
        println!("pq {}", pq.len());

        if curr.x == x_max && curr.y == y_max {
            pq = pq
                .into_iter()
                .filter(|x| x.0.heat_lost.0 < curr.heat_lost.0)
                .collect();
            if curr.heat_lost.0 > total_min_heat_lost {
                continue;
            }
        }

        if let Some(min_heat_lost) = memo.get_mut(&(curr.x, curr.y)) {
            if *min_heat_lost < curr.heat_lost.0 {
                continue;
            }
        }

        let mut nexts = Vec::new();
        let mut new_heat_lost = curr.heat_lost.0;

        // Calculate the range of possible positions that the crucible can move to
        let positions = match curr.direction {
            Direction::Up => {
                let lower = curr.y.saturating_add(min_move);
                let upper = curr.y.saturating_add(max_move);
                // let curry_right: Box<dyn Fn(usize) -> (usize, usize)> = Box::new(|i| (i, y));
                (lower..(upper + 1)).map(Side::Left.curry(curr.x))
            }
            Direction::Down => {
                let lower = curr.y.saturating_sub(max_move);
                let upper = curr.y.saturating_sub(min_move);
                // let curry_right: Box<dyn Fn(usize) -> (usize, usize)> = Box::new(|i| (i, y));
                (lower..upper).map(Side::Left.curry(curr.x))
            }
            Direction::Right => {
                let lower = curr.x.saturating_add(min_move);
                let upper = curr.x.saturating_add(max_move);
                // let curry_left: Box<dyn Fn(usize) -> (usize, usize)> = Box::new(|i| (x, i));
                (lower..(upper + 1)).map(Side::Right.curry(curr.y))
            }
            Direction::Left => {
                let lower = curr.x.saturating_sub(max_move);
                let upper = curr.x.saturating_sub(min_move);
                // let curry_left: Box<dyn Fn(usize) -> (usize, usize)> = Box::new(|i| (x, i));
                (lower..upper).map(Side::Right.curry(curr.y))
            }
        };
        for (nx, ny) in positions {
            println!("{nx}, {ny}");
            if let Some(heat_loss) = grid.get(&(nx, ny)) {
                new_heat_lost += heat_loss;
                if let Some(min_heat_lost) = memo.get_mut(&(nx, ny)) {
                    if *min_heat_lost < new_heat_lost {
                        *min_heat_lost = new_heat_lost;
                        total_min_heat_lost = min(total_min_heat_lost, *min_heat_lost);
                        for new_direction in curr.direction.next_directions() {
                            let c = Crucible::new(nx, ny, new_direction, new_heat_lost);
                            nexts.push((c, c))
                        }
                    }
                } else {
                    for new_direction in curr.direction.next_directions() {
                        let c = Crucible::new(nx, ny, new_direction, new_heat_lost);
                        memo.insert((nx, ny), new_heat_lost);
                        nexts.push((c, c))
                    }
                }
            }
        }
        println!("nexts {}", nexts.len());
        pq.extend(nexts);
        // panic!();
    }

    *memo.get(&(x_max, y_max)).unwrap() as usize
}

pub fn part1() -> usize {
    let input = read_to_string("input/17").unwrap();
    // let input = EXAMPLE;
    min_heat(&input, 1, 3)
}

pub fn part2() -> usize {
    // let input = read_to_string("input/17").unwrap();
    let input = EXAMPLE;
    min_heat(&input, 4, 10)
}
