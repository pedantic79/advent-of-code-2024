use ahash::{AHasher, HashMapExt};
use aoc_runner_derive::{aoc, aoc_generator};
use dashmap::DashMap;
use itertools::{Itertools, iterate};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use rustc_hash::FxHashMap as HashMap;
use std::hash::BuildHasherDefault;

use crate::common::parse::parse_lines;

#[aoc_generator(day22)]
pub fn generator(input: &str) -> Vec<u64> {
    parse_lines(input)
}

fn secret_number(mut s: u64) -> u64 {
    s = ((s * 64) ^ s) % 16777216;
    s = ((s / 32) ^ s) % 16777216;
    s = ((s * 2048) ^ s) % 16777216;
    s
}

#[aoc(day22, part1)]
pub fn part1(inputs: &[u64]) -> u64 {
    inputs
        .par_iter()
        .map(|n| iterate(*n, |s| secret_number(*s)).nth(2000).unwrap())
        .sum()
}

#[aoc(day22, part2)]
pub fn part2(inputs: &[u64]) -> u64 {
    let seqs = inputs.par_iter().map(|n| {
        iterate(*n, |s| secret_number(*s))
            .map(|n| n % 10)
            .take(2001)
            .collect_vec()
    });

    let all = DashMap::with_hasher(BuildHasherDefault::<AHasher>::default());
    seqs.for_each(|seq| {
        let mut monkey = HashMap::new();

        let diff_windows = seq
            .iter()
            .tuple_windows()
            .map(|(&x, &y)| y as i8 - x as i8)
            .tuple_windows()
            .enumerate();

        for (i, pattern @ (_, _, _, _)) in diff_windows {
            monkey.entry(pattern).or_insert(seq[i + 4]);
        }

        for (&k, v) in monkey.iter() {
            *all.entry(k).or_insert(0) += v;
        }
    });

    all.iter().map(|x| *x.value()).max().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE1: &str = r"1
10
100
2024";

    const SAMPLE2: &str = r"1
2
3
2024";

    #[test]
    pub fn input_test() {
        println!("{:?}", generator(SAMPLE1));

        // assert_eq!(generator(SAMPLE), Object());
    }

    #[test]
    pub fn part1_test() {
        assert_eq!(part1(&generator(SAMPLE1)), 37327623);
    }

    #[test]
    pub fn part2_test() {
        assert_eq!(part2(&generator(SAMPLE2)), 23);
    }

    mod regression {
        use super::*;

        const INPUT: &str = include_str!("../input/2024/day22.txt");
        const ANSWERS: (u64, u64) = (16619522798, 1854);

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            let output = generator(input);

            assert_eq!(part1(&output), ANSWERS.0);
            assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
