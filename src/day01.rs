use ahash::HashMapExt;
use aoc_runner_derive::{aoc, aoc_generator};
use rustc_hash::FxHashMap as HashMap;

#[derive(Debug, PartialEq, Eq)]
pub struct Object {
    left: Vec<usize>,
    rite: Vec<usize>,
}

#[aoc_generator(day1)]
pub fn generator(input: &str) -> (Vec<usize>, Vec<usize>) {
    let mut a = Vec::new();
    let mut b = Vec::new();
    for l in input.lines() {
        let (left, rite) = l.split_once("   ").unwrap();
        a.push(left.parse().unwrap());
        b.push(rite.parse().unwrap());
    }
    a.sort_unstable();
    b.sort_unstable();
    (a, b)
}

#[aoc(day1, part1)]
pub fn part1((left, rite): &(Vec<usize>, Vec<usize>)) -> usize {
    left.iter()
        .zip(rite.iter())
        .map(|(&l, &r)| l.abs_diff(r))
        .sum()
}

#[aoc(day1, part2)]
pub fn part2((left, rite): &(Vec<usize>, Vec<usize>)) -> usize {
    let mut freq = HashMap::new();
    for r in rite {
        *freq.entry(*r).or_insert(0) += 1;
    }

    left.iter().map(|&l| l * freq.get(&l).unwrap_or(&0)).sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r"3   4
4   3
2   5
1   3
3   9
3   3";

    #[test]
    pub fn input_test() {
        println!("{:?}", generator(SAMPLE));

        // assert_eq!(generator(SAMPLE), Object());
    }

    #[test]
    pub fn part1_test() {
        assert_eq!(part1(&generator(SAMPLE)), 11);
    }

    #[test]
    pub fn part2_test() {
        assert_eq!(part2(&generator(SAMPLE)), 31);
    }

    mod regression {
        use super::*;

        const INPUT: &str = include_str!("../input/2024/day1.txt");
        const ANSWERS: (usize, usize) = (1258579, 23981443);

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            let output = generator(input);

            assert_eq!(part1(&output), ANSWERS.0);
            assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
