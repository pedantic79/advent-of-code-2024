use aoc_runner_derive::{aoc, aoc_generator};
use itertools::Itertools;
use pathfinding::{matrix::directions::DIRECTIONS_4, prelude::astar_bag, utils::move_in_direction};
use rustc_hash::FxHashMap as HashMap;

type Pad<'a> = &'a [&'a [u8]];
type Position = (usize, usize);

const NUM_PAD: Pad = &[b"789", b"456", b"123", b".0A"];
const DIR_PAD: Pad = &[b".^A", b"<v>"];
const NUM_POS: &[Position] = &[
    (3, 1),
    (2, 0),
    (2, 1),
    (2, 2),
    (1, 0),
    (1, 1),
    (1, 2),
    (0, 0),
    (0, 1),
    (0, 2),
    (3, 2),
];
const DIR_POS: &[Position] = &[(0, 1), (0, 2), (1, 0), (1, 1), (1, 2)];

fn char2pos(pad: Pad, c: u8) -> Position {
    if pad.len() == NUM_PAD.len() {
        NUM_POS[if c.is_ascii_digit() {
            usize::from(c - b'0')
        } else {
            10
        }]
    } else {
        DIR_POS[match c {
            b'^' => 0,
            b'A' => 1,
            b'<' => 2,
            b'v' => 3,
            b'>' => 4,
            _ => panic!("unknown char: {}", c as char),
        }]
    }
}

fn pos2dir(positions: &[Position]) -> Vec<u8> {
    let mut dirs: Vec<_> = positions
        .windows(2)
        .map(|x| {
            let (a0, a1) = (x[0].0 as isize, x[0].1 as isize);
            let (b0, b1) = (x[1].0 as isize, x[1].1 as isize);

            (b0 - a0, b1 - a1)
        })
        .map(|dir| match dir {
            (0, 1) => b'>',
            (1, 0) => b'v',
            (0, -1) => b'<',
            (-1, 0) => b'^',
            _ => panic!("unknown direction"),
        })
        .collect();

    dirs.push(b'A');
    dirs
}

fn get_paths(pad: Pad, a: u8, b: u8) -> impl Iterator<Item = Vec<u8>> {
    let a = char2pos(pad, a);
    let b = char2pos(pad, b);

    astar_bag(
        &a,
        |&pos| {
            DIRECTIONS_4
                .into_iter()
                .filter_map(move |dir| move_in_direction(pos, dir, (pad.len(), pad[0].len())))
                .filter(|new_pos| pad[new_pos.0][new_pos.1] != b'.')
                .map(|new_pos| (new_pos, 1))
        },
        |_| 0,
        |&pos| pos == b,
    )
    .unwrap()
    .0
    .map(|v| pos2dir(&v))
}

fn shortest_len(
    seq: &[u8],
    depth: usize,
    max_depth: usize,
    memo: &mut HashMap<(usize, Vec<u8>), usize>,
) -> usize {
    if let Some(&len) = memo.get(&(depth, seq.to_vec())) {
        return len;
    }

    let pad = if depth == 0 { NUM_PAD } else { DIR_PAD };
    let len = std::iter::once(b'A')
        .chain(seq.iter().copied())
        .tuple_windows()
        .map(|(a, b)| {
            let paths = get_paths(pad, a, b);
            if depth == max_depth {
                paths.map(|path| path.len()).min().unwrap()
            } else {
                paths
                    .map(|path| shortest_len(&path, depth + 1, max_depth, memo))
                    .min()
                    .unwrap()
            }
        })
        .sum();

    memo.insert((depth, seq.to_vec()), len);

    len
}

#[aoc_generator(day21)]
pub fn generator(input: &str) -> Vec<String> {
    input.lines().map(ToOwned::to_owned).collect()
}

fn solve<const MAX: usize>(inputs: &[String]) -> usize {
    let mut memo = Default::default();
    inputs
        .iter()
        .map(|seq| {
            let n = seq[..seq.len() - 1].parse::<usize>().unwrap();
            let l = shortest_len(seq.as_bytes(), 0, MAX, &mut memo);
            n * l
        })
        .sum()
}

#[aoc(day21, part1)]
pub fn part1(inputs: &[String]) -> usize {
    solve::<2>(inputs)
}

#[aoc(day21, part2)]
pub fn part2(inputs: &[String]) -> usize {
    solve::<25>(inputs)
}

pub fn debug_output(v: &[u8]) -> &str {
    unsafe { std::str::from_utf8_unchecked(v) }
}

#[cfg(test)]
mod tests {

    use super::*;

    const SAMPLE: &str = r"029A
980A
179A
456A
379A";

    #[test]
    pub fn input_test() {
        println!("{:?}", generator(SAMPLE));
    }

    #[test]
    pub fn part1_test() {
        assert_eq!(part1(&["279A".to_string()]), 279 * 72);
        assert_eq!(part1(&["341A".to_string()]), 341 * 72);
        assert_eq!(part1(&["459A".to_string()]), 459 * 74);
        assert_eq!(part1(&generator(SAMPLE)), 126384);
    }

    #[test]
    pub fn part2_test() {
        assert_eq!(part2(&generator(SAMPLE)), 154115708116294);
    }

    mod regression {
        use super::*;

        const INPUT: &str = include_str!("../input/2024/day21.txt");
        const ANSWERS: (usize, usize) = (123096, 154517692795352);

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            let output = generator(input);

            assert_eq!(part1(&output), ANSWERS.0);
            assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
