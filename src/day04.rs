use aoc_runner_derive::{aoc, aoc_generator};
use pathfinding::matrix::{
    Matrix,
    directions::{self},
};
use smallstr::SmallString;

#[aoc_generator(day4)]
pub fn generator(input: &str) -> Matrix<u8> {
    input.lines().map(|line| line.bytes()).collect()
}

fn get(matrix: &Matrix<u8>, pos: (usize, usize), direction: (isize, isize)) -> Option<u8> {
    matrix
        .get(matrix.move_in_direction(pos, direction)?)
        .copied()
}

fn check_mas(matrix: &Matrix<u8>, pos: (usize, usize), direction: (isize, isize)) -> bool {
    let mut m = matrix
        .in_direction(pos, direction)
        .map(|p| matrix.get(p).copied().unwrap_or(b' '));

    let s = unsafe {
        SmallString::from_buf_unchecked([
            b'X',
            m.next().unwrap_or(b' '),
            m.next().unwrap_or(b' '),
            m.next().unwrap_or(b' '),
        ])
    };

    s == "XMAS"
}

#[aoc(day4, part1)]
pub fn part1(matrix: &Matrix<u8>) -> usize {
    let mut count = 0;

    for pos in matrix.keys() {
        if Some(&b'X') == matrix.get(pos) {
            count += directions::DIRECTIONS_8
                .iter()
                .filter(|&&dir| check_mas(matrix, pos, dir))
                .count();
        }
    }

    count
}

fn get_corners(matrix: &Matrix<u8>, pos: (usize, usize)) -> SmallString<[u8; 4]> {
    // SAFETY: Everything is unicode
    unsafe {
        SmallString::from_buf_unchecked([
            get(matrix, pos, directions::NE).unwrap_or(b' '),
            get(matrix, pos, directions::SE).unwrap_or(b' '),
            get(matrix, pos, directions::SW).unwrap_or(b' '),
            get(matrix, pos, directions::NW).unwrap_or(b' '),
        ])
    }
}

#[aoc(day4, part2)]
pub fn part2(matrix: &Matrix<u8>) -> usize {
    let mut count = 0;

    for pos in matrix.keys() {
        if Some(&b'A') == matrix.get(pos) {
            let v = get_corners(matrix, pos);
            count += usize::from(v == "MMSS" || v == "MSSM" || v == "SSMM" || v == "SMMS");
        }
    }

    count
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r"MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX";

    #[test]
    pub fn input_test() {
        println!("{:?}", generator(SAMPLE));

        // assert_eq!(generator(SAMPLE), Object());
    }

    #[test]
    pub fn part1_test() {
        assert_eq!(part1(&generator(SAMPLE)), 18);
    }

    #[test]
    pub fn part2_test() {
        assert_eq!(part2(&generator(SAMPLE)), 9);
    }

    mod regression {
        use super::*;

        const INPUT: &str = include_str!("../input/2024/day4.txt");
        const ANSWERS: (usize, usize) = (2493, 1890);

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            let output = generator(input);

            assert_eq!(part1(&output), ANSWERS.0);
            assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
