use ahash::{HashMap, HashMapExt, HashSet, HashSetExt};
use aoc_runner_derive::{aoc, aoc_generator};
use itertools::{iterate, Itertools};

#[derive(Debug, PartialEq, Eq)]
pub struct Input {
    map: HashMap<char, Vec<(usize, usize)>>,
    size: (usize, usize),
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

fn checked_add_pos(
    v: (usize, usize),
    x: (usize, usize),
    y: (usize, usize),
) -> Option<(usize, usize)> {
    let a = v.0.checked_add(x.0)?.checked_sub(y.0)?;
    let b = v.1.checked_add(x.1)?.checked_sub(y.1)?;

    Some((a, b))
}

fn generate_resonance(
    a: (usize, usize),
    b: (usize, usize),
    size: (usize, usize),
    map: &mut HashSet<(usize, usize)>,
) {
    if let Some(x) = checked_add_pos(a, a, b) {
        if x.0 < size.0 && x.1 < size.1 {
            map.insert(x);
        }
    }

    if let Some(x) = checked_add_pos(b, b, a) {
        if x.0 < size.0 && x.1 < size.1 {
            map.insert(x);
        }
    }
}

fn generate_resonance2(
    a: (usize, usize),
    b: (usize, usize),
    size: (usize, usize),
    map: &mut HashSet<(usize, usize)>,
) {
    for (y, x) in iterate(a, |v| {
        checked_add_pos(*v, a, b).unwrap_or((usize::MAX, usize::MAX))
    }) {
        if y < size.0 && x < size.1 {
            map.insert((y, x));
        } else {
            break;
        }
    }

    for (y, x) in iterate(b, |v| {
        checked_add_pos(*v, b, a).unwrap_or((usize::MAX, usize::MAX))
    }) {
        if y < size.0 && x < size.1 {
            map.insert((y, x));
        } else {
            break;
        }
    }
}

#[aoc(day8, part1)]
pub fn part1(Input { map, size }: &Input) -> usize {
    let mut points = HashSet::new();
    for pos in map.values() {
        for x in pos.iter().combinations(2) {
            generate_resonance(*x[0], *x[1], *size, &mut points);
        }
    }

    points.len()
}

#[aoc(day8, part2)]
pub fn part2(Input { map, size }: &Input) -> usize {
    let mut points = HashSet::new();
    for pos in map.values() {
        for x in pos.iter().combinations(2) {
            generate_resonance2(*x[0], *x[1], *size, &mut points);
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
        const ANSWERS: (usize, usize) = (413, 1417);

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            let output = generator(input);

            assert_eq!(part1(&output), ANSWERS.0);
            assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
