use std::iter;

use aoc_runner_derive::{aoc, aoc_generator};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Group {
    id: i64,
    len: usize,
}

impl std::fmt::Display for Group {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.len {
            if self.id != -1 {
                write!(f, "{}", self.id)?;
            } else {
                write!(f, ".")?;
            }
        }

        Ok(())
    }
}

impl Group {
    fn new(id: i64, count: usize) -> Self {
        Self { id, len: count }
    }
}

#[aoc_generator(day9)]
pub fn generator(input: &str) -> (Vec<i64>, Vec<Group>) {
    let mut p1 = Vec::new();
    let mut p2 = Vec::new();
    let mut file_id = 0;
    for (c, is_file) in input.chars().zip([true, false].into_iter().cycle()) {
        let n = c.to_digit(10).unwrap() as usize;
        if is_file {
            p1.extend(iter::repeat_n(file_id, n));
            p2.push(Group::new(file_id, n));
            file_id += 1;
        } else {
            p1.extend(iter::repeat_n(-1, n));
            p2.push(Group::new(-1, n));
        }
    }

    (p1, p2)
}

#[aoc(day9, part1)]
pub fn part1(inputs: &(Vec<i64>, Vec<Group>)) -> usize {
    let mut inputs = inputs.0.to_vec();
    let mut len = inputs.len();

    let mut i = 0;
    while i < len {
        if inputs[i] == -1 {
            inputs.swap(i, len - 1);
            inputs.pop();

            // remove some at the end
            while Some(-1) == inputs.last().copied() {
                inputs.pop();
            }
            len = inputs.len();
        }
        i += 1;
    }

    inputs
        .iter()
        .enumerate()
        .map(|(idx, &c)| (c as usize) * idx)
        .sum()
}

pub fn print_window(inputs: &[Group]) {
    for g in inputs {
        print!("{}", g);
    }
    println!();
}

#[aoc(day9, part2)]
pub fn part2(inputs: &(Vec<i64>, Vec<Group>)) -> usize {
    let mut inputs = inputs.1.to_vec();
    let mut len = inputs.len();

    let mut idx = 0;
    while idx < len {
        if inputs[idx].id == -1 {
            for pos in ((idx + 1)..len).rev() {
                if inputs[pos].id != -1 && inputs[pos].len <= inputs[idx].len {
                    inputs.swap(idx, pos);

                    if inputs[pos].len != inputs[idx].len {
                        // fix length
                        let new_len = inputs[pos].len - inputs[idx].len;
                        inputs[pos].len = inputs[idx].len;
                        inputs.insert(idx + 1, Group::new(-1, new_len));
                        len += 1;
                    }

                    // print_window(&inputs);
                    // println!("swapping, {} and {}: ", i, pos);
                    break;
                }
            }
        }
        idx += 1;
    }

    let mut sum = 0;
    let mut pos = 0;

    for Group { id, len } in inputs {
        if id != -1 {
            let total_pos_sum = len * (2 * pos + len - 1) / 2;
            sum += total_pos_sum * (id as usize);
        }
        pos += len;
    }

    sum
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r"2333133121414131402";

    #[test]
    pub fn input_test() {
        println!("{:?}", generator(SAMPLE));

        // assert_eq!(generator(SAMPLE), Object());
    }

    #[test]
    pub fn part1_test() {
        assert_eq!(part1(&generator(SAMPLE)), 1928);
    }

    #[test]
    pub fn part2_test() {
        assert_eq!(part2(&generator(SAMPLE)), 2858);
    }

    mod regression {
        use super::*;

        const INPUT: &str = include_str!("../input/2024/day9.txt");
        const ANSWERS: (usize, usize) = (6288599492129, 6321896265143);

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            let output = generator(input);

            assert_eq!(part1(&output), ANSWERS.0);
            assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
