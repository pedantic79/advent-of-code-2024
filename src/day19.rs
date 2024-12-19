use ahash::{HashMap, HashSet, HashSetExt};
use aoc_runner_derive::{aoc, aoc_generator};

#[derive(Debug, PartialEq, Eq)]
pub struct Input {
    towels: Vec<HashSet<String>>,
    designs: Vec<String>,
}

impl Input {
    fn solve1<'a>(&self, start: &'a str, memo: &mut HashMap<&'a str, bool>) -> bool {
        if memo.contains_key(start) {
            return memo[start];
        }

        for (i, pat) in self.towels.iter().enumerate().skip(1).take(start.len()) {
            let (prefix, postfix) = start.split_at(i);

            if pat.contains(prefix) && self.solve1(postfix, memo) {
                memo.insert(start, true);
                return true;
            }
        }

        memo.insert(start, false);
        false
    }

    fn solve2<'a>(&self, start: &'a str, memo: &mut HashMap<&'a str, usize>) -> usize {
        if memo.contains_key(start) {
            return memo[start];
        }

        let mut combs = 0;
        for (i, pat) in self.towels.iter().enumerate().skip(1).take(start.len()) {
            let (prefix, postfix) = start.split_at(i);

            if pat.contains(prefix) {
                combs += self.solve2(postfix, memo);
            }
        }

        memo.insert(start, combs);
        combs
    }
}

#[aoc_generator(day19)]
pub fn generator(input: &str) -> Input {
    let (a, b) = input.split_once("\n\n").unwrap();
    let mut towels = Vec::new();

    for s in a.split(", ") {
        let s = s.to_string();
        let len = s.len();
        let mut towel_len = towels.len();

        // make sure we have enough two
        while len + 1 > towel_len {
            towels.push(HashSet::new());
            towel_len += 1;
        }
        assert!(towels.len() > s.len());

        towels[len].insert(s);
    }

    let designs = b.lines().map(|s| s.to_string()).collect();

    Input { towels, designs }
}

#[aoc(day19, part1)]
pub fn part1(inputs: &Input) -> usize {
    let mut cache = HashMap::default();
    cache.insert("", true);
    inputs
        .designs
        .iter()
        .filter(|t| inputs.solve1(t, &mut cache))
        .count()
}

#[aoc(day19, part2)]
pub fn part2(inputs: &Input) -> usize {
    let mut cache = HashMap::default();
    cache.insert("", 1);
    inputs
        .designs
        .iter()
        .map(|t| inputs.solve2(t, &mut cache))
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r"r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb";

    #[test]
    pub fn input_test() {
        println!("{:?}", generator(SAMPLE));

        // assert_eq!(generator(SAMPLE), Object());
    }

    #[test]
    pub fn part1_test() {
        assert_eq!(part1(&generator(SAMPLE)), 6);
    }

    #[test]
    pub fn part2_test() {
        assert_eq!(part2(&generator(SAMPLE)), 16);
    }

    mod regression {
        use super::*;

        const INPUT: &str = include_str!("../input/2024/day19.txt");
        const ANSWERS: (usize, usize) = (319, 692575723305545);

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            let output = generator(input);

            assert_eq!(part1(&output), ANSWERS.0);
            assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
