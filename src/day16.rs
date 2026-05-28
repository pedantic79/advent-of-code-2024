use aoc_runner_derive::{aoc, aoc_generator};
use bit_set::BitSet;
use pathfinding::{
    matrix::directions,
    prelude::{astar_bag, dijkstra},
    utils::move_in_direction,
};

type Direction = (isize, isize);
type Position = (usize, usize);
type State = (Position, Direction);

#[derive(Debug, PartialEq, Eq)]
pub struct Maze {
    start: (Position, Direction),
    end: Position,
    map: Vec<Vec<u8>>,
}

#[aoc_generator(day16)]
pub fn generator(input: &str) -> Maze {
    let mut start = (0, 0);
    let mut end = (0, 0);

    let map = input
        .lines()
        .enumerate()
        .map(|(r, line)| {
            if let Some(c) = line.bytes().position(|b| b == b'E') {
                end = (r, c)
            } else if let Some(c) = line.bytes().position(|b| b == b'S') {
                start = (r, c)
            }

            line.trim_end().to_string().into_bytes()
        })
        .collect();

    Maze {
        start: (start, directions::E),
        end,
        map,
    }
}

fn choices((y, x): Direction) -> [(Direction, usize); 3] {
    // ((-y, -x), 2000), // this is going backwards

    [((y, x), 1), ((x, -y), 1000), ((-x, y), 1000)]
}

fn successors(maze: &Maze, state: State) -> impl Iterator<Item = (State, usize)> + '_ {
    let (pos, dir) = state;

    choices(dir).into_iter().filter_map(move |(new_dir, cost)| {
        if cost == 1 {
            let new_pos = move_in_direction(pos, dir, (maze.map.len(), maze.map[0].len()))?;
            if maze.map[new_pos.0][new_pos.1] != b'#' {
                Some(((new_pos, dir), cost))
            } else {
                None
            }
        } else {
            Some(((pos, new_dir), cost))
        }
    })
}

#[aoc(day16, part1)]
pub fn part1(maze: &Maze) -> usize {
    dijkstra(
        &maze.start,
        |&state| successors(maze, state),
        |&(pos, _)| pos == maze.end,
    )
    .unwrap()
    .1
}

#[aoc(day16, part2)]
pub fn part2(maze: &Maze) -> usize {
    let width = maze.map[0].len();

    astar_bag(
        &maze.start,
        |&state| successors(maze, state),
        |_| 0,
        |&(pos, _)| pos == maze.end,
    )
    .unwrap()
    .0
    .flat_map(|v| v.into_iter().map(|(p, _)| p.0 * width + p.1))
    .collect::<BitSet<usize>>()
    .count()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r"###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############";

    const SAMPLE1: &str = r"#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################";

    #[test]
    pub fn input_test() {
        println!("{:?}", generator(SAMPLE));

        // assert_eq!(generator(SAMPLE), Object());
    }

    #[test]
    pub fn part1_test() {
        assert_eq!(part1(&generator(SAMPLE)), 7036);
        assert_eq!(part1(&generator(SAMPLE1)), 11048);
    }

    #[test]
    pub fn part2_test() {
        assert_eq!(part2(&generator(SAMPLE)), 45);
        assert_eq!(part2(&generator(SAMPLE1)), 64);
    }

    mod regression {
        use super::*;

        const INPUT: &str = include_str!("../input/2024/day16.txt");
        const ANSWERS: (usize, usize) = (99460, 500);

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            let output = generator(input);

            assert_eq!(part1(&output), ANSWERS.0);
            assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
