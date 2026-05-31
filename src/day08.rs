use std::iter::successors;

use ahash::{HashMapExt, HashSetExt};
use aoc_runner_derive::{aoc, aoc_generator};
use itertools::Itertools;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

type Point = (usize, usize);

#[derive(Debug, PartialEq, Eq)]
pub struct Input {
    map: HashMap<char, Vec<Point>>,
    size: Point,
}

#[aoc_generator(day8)]
pub fn generator(input: &str) -> Input {
    let mut map = HashMap::new();
    for (y, row) in input.lines().enumerate() {
        for (x, cell) in row.bytes().enumerate() {
            if cell == b'.' || cell == b' ' {
                continue;
            }

            map.entry(cell as char).or_insert(Vec::new()).push((y, x));
        }
    }

    Input {
        map,
        size: (input.lines().count(), input.lines().next().unwrap().len()),
    }
}

fn checked_add_pos(v: Point, x: Point, y: Point, size: Point) -> Option<Point> {
    pathfinding::utils::move_in_direction(
        v,
        (x.0.checked_signed_diff(y.0)?, x.1.checked_signed_diff(y.1)?),
        size,
    )
}

#[aoc(day8, part1)]
pub fn part1(Input { map, size }: &Input) -> usize {
    let mut points = HashSet::new();
    for pos in map.values() {
        for x in pos.iter().combinations(2) {
            let (a, b) = (*x[0], *x[1]);

            points.extend(checked_add_pos(a, a, b, *size));
            points.extend(checked_add_pos(b, b, a, *size));
        }
    }

    points.len()
}

#[aoc(day8, part2)]
pub fn part2(Input { map, size }: &Input) -> usize {
    let mut points = HashSet::new();
    for pos in map.values() {
        for x in pos.iter().combinations(2) {
            let (a, b) = (*x[0], *x[1]);
            points.extend(successors(Some(b), |&v| checked_add_pos(v, b, a, *size)));
            points.extend(successors(Some(a), |&v| checked_add_pos(v, a, b, *size)));
        }
    }

    points.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r"............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............";

    #[test]
    pub fn input_test() {
        println!("{:?}", generator(SAMPLE));

        // assert_eq!(generator(SAMPLE), Object());
    }

    #[test]
    pub fn part1_test() {
        assert_eq!(part1(&generator(SAMPLE)), 14);
    }

    #[test]
    pub fn part2_test() {
        assert_eq!(part2(&generator(SAMPLE)), 34);
    }

    mod regression {
        use super::*;

        const INPUT: &str = include_str!("../input/2024/day8.txt");
        const ANSWERS: Point = (413, 1417);

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            let output = generator(input);

            assert_eq!(part1(&output), ANSWERS.0);
            assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
