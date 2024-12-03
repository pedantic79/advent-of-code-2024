use aoc_runner_derive::{aoc, aoc_generator};

#[derive(Debug, PartialEq, Eq)]
pub struct Object {}

#[aoc_generator(dayN)]
pub fn generator(input: &str) -> Object {
    unimplemented!()
}

#[aoc(dayN, part1)]
pub fn part1(inputs: &Object) -> usize {
    unimplemented!()
}

#[aoc(dayN, part2)]
pub fn part2(inputs: &Object) -> usize {
    unimplemented!()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r"..##.......";

    #[test]
    pub fn input_test() {
        println!("{:?}", generator(SAMPLE));

        // assert_eq!(generator(SAMPLE), Object());
    }

    #[test]
    pub fn part1_test() {
        // assert_eq!(part1(&generator(SAMPLE)), 7);
    }

    #[test]
    pub fn part2_test() {
        // assert_eq!(part2(&generator(SAMPLE)), 336);
    }

    mod regression {
        use super::*;

        const INPUT: &str = include_str!("../input/2024/dayN.txt");
        const ANSWERS: (usize, usize) = (0, 0);

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            // let output = generator(input);

            // assert_eq!(part1(&output), ANSWERS.0);
            // assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
