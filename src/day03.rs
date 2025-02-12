use aoc_runner_derive::{aoc, aoc_generator};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::anychar,
    combinator::map,
    multi::{many0, many_till},
    sequence, IResult, Parser,
};

use crate::common::nom::nom_usize;

#[derive(Debug, PartialEq, Eq)]
pub enum Symbols {
    Mul(usize),
    Do,
    Dont,
}

fn parse_symbol(s: &str) -> IResult<&str, Symbols> {
    let symbols = alt((
        map(tag("don't()"), |_| Symbols::Dont),
        map(tag("do()"), |_| Symbols::Do),
        map(
            sequence::delimited(tag("mul("), parse_mul, tag(")")),
            Symbols::Mul,
        ),
    ));

    // skips junk until symbols produces a result
    let (s, (_, mul)) = many_till(anychar, symbols).parse(s)?;
    Ok((s, mul))
}

fn parse_mul(s: &str) -> IResult<&str, usize> {
    let (rest, a) = nom_usize(s)?;
    let (rest, _) = tag(",")(rest)?;
    let (rest, b) = nom_usize(rest)?;

    Ok((rest, a * b))
}

#[aoc_generator(day3)]
pub fn generator(input: &str) -> Vec<Symbols> {
    many0(parse_symbol).parse(input).unwrap().1
}

#[aoc(day3, part1)]
pub fn part1(inputs: &[Symbols]) -> usize {
    inputs
        .iter()
        .map(|i| if let Symbols::Mul(x) = i { *x } else { 0 })
        .sum()
}

#[aoc(day3, part2)]
pub fn part2(inputs: &[Symbols]) -> usize {
    let mut enabled = true;
    inputs
        .iter()
        .map(|i| {
            match i {
                Symbols::Mul(x) if enabled => return *x,
                Symbols::Do => enabled = true,
                Symbols::Dont => enabled = false,
                _ => {}
            }
            0
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r"xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
    const SAMPLE2: &str =
        r"xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";

    #[test]
    pub fn input_test() {
        println!("{:?}", generator(SAMPLE));

        // assert_eq!(generator(SAMPLE), Object());
    }

    #[test]
    pub fn part1_test() {
        assert_eq!(part1(&generator(SAMPLE)), 161);
    }

    #[test]
    pub fn part2_test() {
        assert_eq!(part2(&generator(SAMPLE2)), 48);
    }

    mod regression {
        use super::*;

        const INPUT: &str = include_str!("../input/2024/day3.txt");
        const ANSWERS: (usize, usize) = (182780583, 90772405);

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            let output = generator(input);

            assert_eq!(part1(&output), ANSWERS.0);
            assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
