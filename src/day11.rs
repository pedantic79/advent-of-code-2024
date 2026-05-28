use std::{iter, mem};

use aoc_runner_derive::{aoc, aoc_generator};
use rustc_hash::FxHashMap as HashMap;

use crate::common::parse::parse_split;

#[aoc_generator(day11)]
pub fn generator(input: &str) -> Vec<u64> {
    parse_split(input, ' ')
}

fn one_or_two_elements<T>(first: T, second: Option<T>) -> impl Iterator<Item = T> {
    iter::once(first).chain(iter::once(second).flatten())
}

fn even_digits(n: u64) -> Option<(u64, u64)> {
    match n {
        10..100 => Some((n / 10, n % 10)),
        1000..10000 => Some((n / 100, n % 100)),
        100000..1000000 => Some((n / 1000, n % 1000)),
        10000000..100000000 => Some((n / 10000, n % 10000)),
        1000000000..10000000000 => Some((n / 100000, n % 100000)),
        100000000000..1000000000000 => Some((n / 1000000, n % 1000000)),
        10000000000000..100000000000000 => Some((n / 10000000, n % 10000000)),
        1000000000000000..10000000000000000 => Some((n / 100000000, n % 100000000)),
        100000000000000000..1000000000000000000 => Some((n / 1000000000, n % 1000000000)),
        10000000000000000000..=u64::MAX => Some((n / 10000000000, n % 10000000000)),
        _ => None,
    }
}

fn rule(stone: u64) -> impl Iterator<Item = u64> {
    if stone == 0 {
        one_or_two_elements(1, None)
    } else {
        // let s = format!("{}", stone);
        if let Some((l, r)) = even_digits(stone) {
            one_or_two_elements(l, Some(r))
        } else {
            one_or_two_elements(stone * 2024, None)
        }
    }
}

pub fn solve_step(stone: u64, rem: u64, memo: &mut HashMap<(u64, u64), usize>) -> usize {
    if let Some(c) = memo.get(&(stone, rem)) {
        return *c;
    }
    if rem == 0 {
        return 1;
    }

    let count = rule(stone).map(|x| solve_step(x, rem - 1, memo)).sum();
    memo.insert((stone, rem), count);
    count
}

fn solve<const T: usize>(stones: &[u64]) -> usize {
    let mut data = HashMap::default();
    let mut counts = HashMap::default();
    for &stone in stones {
        *data.entry(stone).or_insert(0) += 1;
    }
    for _ in 0..T {
        for (&stone, &stone_count) in data.iter() {
            for a in rule(stone) {
                *counts.entry(a).or_insert(0) += stone_count;
            }
        }
        mem::swap(&mut data, &mut counts);
        counts.clear();
    }

    data.values().sum()
}

#[aoc(day11, part1)]
pub fn part1(inputs: &[u64]) -> usize {
    solve::<25>(inputs)
    // let mut memo = HashMap::default();
    // inputs.iter().map(|&x| solve_step(x, 25, &mut memo)).sum()
}

#[aoc(day11, part2)]
pub fn part2(inputs: &[u64]) -> usize {
    solve::<75>(inputs)
    // let mut memo = HashMap::default();
    // inputs.iter().map(|&x| solve_step(x, 75, &mut memo)).sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r"125 17";

    #[test]
    pub fn input_test() {
        println!("{:?}", generator(SAMPLE));

        // assert_eq!(generator(SAMPLE), Object());
    }

    #[test]
    pub fn part1_test() {
        assert_eq!(part1(&generator(SAMPLE)), 55312);
    }

    #[test]
    pub fn part2_test() {
        assert_eq!(part2(&generator(SAMPLE)), 65601038650482);
    }

    mod regression {
        use super::*;

        const INPUT: &str = include_str!("../input/2024/day11.txt");
        const ANSWERS: (usize, usize) = (183248, 218811774248729);

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            let output = generator(input);

            assert_eq!(part1(&output), ANSWERS.0);
            assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
