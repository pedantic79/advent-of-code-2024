use aoc_runner_derive::{aoc, aoc_generator};
use nom::{IResult, bytes::complete::tag, character::complete::newline};

use crate::common::nom::nom_i64;

#[derive(Debug, PartialEq, Eq)]
pub struct Machine {
    a: (i64, i64),
    b: (i64, i64),
    prize: (i64, i64),
}

impl Machine {
    fn solve_linear_system(a: (i64, i64), b: (i64, i64), p: (i64, i64)) -> Option<(i64, i64)> {
        let det = a.0 * b.1 - a.1 * b.0;
        if det == 0 {
            return None;
        }

        let aa = (p.0 * b.1 - p.1 * b.0) / det;
        let bb = (p.1 * a.0 - p.0 * a.1) / det;

        if a.0 * aa + b.0 * bb == p.0 && a.1 * aa + b.1 * bb == p.1 {
            Some((aa, bb))
        } else {
            None
        }
    }

    fn find_cheapest<const FUDGE: i64>(&self) -> i64 {
        let p = (self.prize.0 + FUDGE, self.prize.1 + FUDGE);

        Self::solve_linear_system(self.a, self.b, p)
            .map(|(a, b)| 3 * a + b)
            .unwrap_or_default()
    }
}

fn parse_machine(s: &str) -> IResult<&str, Machine> {
    let (s, _) = tag("Button A: X+")(s)?;
    let (s, ax) = nom_i64(s)?;
    let (s, _) = tag(", Y+")(s)?;
    let (s, ay) = nom_i64(s)?;
    let (s, _) = newline(s)?;

    let (s, _) = tag("Button B: X+")(s)?;
    let (s, bx) = nom_i64(s)?;
    let (s, _) = tag(", Y+")(s)?;
    let (s, by) = nom_i64(s)?;
    let (s, _) = newline(s)?;

    let (s, _) = tag("Prize: X=")(s)?;
    let (s, px) = nom_i64(s)?;
    let (s, _) = tag(", Y=")(s)?;
    let (s, py) = nom_i64(s)?;

    Ok((
        s,
        Machine {
            a: (ay, ax),
            b: (by, bx),
            prize: (py, px),
        },
    ))
}

#[aoc_generator(day13)]
pub fn generator(input: &str) -> Vec<Machine> {
    input
        .split("\n\n")
        .map(|block| parse_machine(block).unwrap().1)
        .collect()
}

#[aoc(day13, part1)]
pub fn part1(inputs: &[Machine]) -> i64 {
    inputs.iter().map(|m| m.find_cheapest::<0>()).sum()
}

#[aoc(day13, part2)]
pub fn part2(inputs: &[Machine]) -> i64 {
    inputs
        .iter()
        .map(|m| m.find_cheapest::<10000000000000>())
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r"Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279";

    #[test]
    pub fn input_test() {
        println!("{:?}", generator(SAMPLE));

        // assert_eq!(generator(SAMPLE), Object());
    }

    #[test]
    pub fn part1_test() {
        assert_eq!(part1(&generator(SAMPLE)), 480);
    }

    #[test]
    pub fn part2_test() {
        assert_eq!(part2(&generator(SAMPLE)), 875318608908);
    }

    mod regression {
        use super::*;

        const INPUT: &str = include_str!("../input/2024/day13.txt");
        const ANSWERS: (i64, i64) = (29436, 103729094227877);

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            let output = generator(input);

            assert_eq!(part1(&output), ANSWERS.0);
            assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
