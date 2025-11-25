use std::fmt::Write;

use aoc_runner_derive::{aoc, aoc_generator};
use nom::{IResult, Parser, bytes::complete::tag, sequence::preceded};

use crate::common::parse::parse_split;

type IntType = u64;

fn nom_int_type(s: &str) -> IResult<&str, IntType> {
    crate::common::nom::nom_u64.parse(s)
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Computer {
    a: IntType,
    b: IntType,
    c: IntType,
    output: Vec<IntType>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum Instruction {
    Adv = 0,
    Bxl = 1,
    Bst = 2,
    Jnz = 3,
    Bxc = 4,
    Out = 5,
    Bdv = 6,
    Cdv = 7,
}

impl Computer {
    fn new(a: IntType) -> Self {
        Self {
            a,
            b: 0,
            c: 0,
            output: Vec::with_capacity(16),
        }
    }

    fn run_instruction(&mut self, ins: IntType, operand: IntType) -> Option<usize> {
        let combo = self.get_combo(operand);

        // SAFETY: transmute is safe because each Instruction is represented by a u8
        // the input is guaranteed to be a u8 between 0 and 7
        let ins = unsafe { std::mem::transmute::<u8, Instruction>(ins as u8) };

        match ins {
            Instruction::Adv => {
                self.a >>= combo;
            }
            Instruction::Bxl => {
                self.b ^= operand;
            }
            Instruction::Bst => {
                self.b = combo & 7;
            }
            Instruction::Jnz => {
                if self.a != 0 {
                    return Some(operand as usize);
                }
            }
            Instruction::Bxc => {
                self.b ^= self.c;
            }
            Instruction::Out => {
                self.output.push(combo & 7);
            }
            Instruction::Bdv => {
                self.b = self.a >> combo;
            }
            Instruction::Cdv => {
                self.c = self.a >> combo;
            }
        }

        None
    }

    fn get_combo(&self, operand: IntType) -> IntType {
        match operand {
            0..4 => operand,
            4 => self.a,
            5 => self.b,
            6 => self.c,
            _ => panic!("invalid operand"),
        }
    }
}

fn parse_a(s: &str) -> IResult<&str, IntType> {
    let (s, a) = preceded(tag("Register A: "), nom_int_type).parse(s)?;

    Ok((s, a))
}

#[aoc_generator(day17)]
pub fn generator(input: &str) -> (IntType, Vec<IntType>) {
    let (reg, pro) = input.split_once("\n\n").unwrap();

    let a = parse_a.parse(reg).unwrap().1;
    let nums = pro.split_once(": ").unwrap().1;
    let program = parse_split(nums, ',');

    (a, program)
}

fn run_computer(a: IntType, ins: &[IntType], one_shot: bool) -> Vec<IntType> {
    let mut computer = Computer::new(a);

    let mut pc = 0;
    while pc + 1 < ins.len() {
        let i = ins[pc];
        let j = ins[pc + 1];

        if let Some(jump) = computer.run_instruction(i, j) {
            // if we are set to loop only once, then immediately return the output when we get to the jump
            if one_shot {
                assert_eq!(computer.output.len(), 1);
                return computer.output;
            }
            pc = jump;
        } else {
            pc += 2;
        }
    }

    computer.output
}

#[aoc(day17, part1)]
pub fn part1((a, program): &(IntType, Vec<IntType>)) -> String {
    let output = run_computer(*a, program, false);

    // This avoids allocating a vec, and extra strings.
    let mut buf = String::with_capacity(output.len() * 2 - 1);
    buf.write_fmt(format_args!("{}", output[0])).unwrap();
    for n in &output[1..] {
        buf.write_fmt(format_args!(",{n}")).unwrap();
    }
    buf
}

fn solve_p2(a: IntType, target: &[IntType], ins: &[IntType]) -> Option<IntType> {
    if target.is_empty() {
        // For whatever reason, if a == 0 and i == 0 produces a solution, it is wrong.
        // I don't know if others see this, but this is the most reliable method of checking
        // the filter can be removed if you include the check for a == 0 and i == 0
        return Some(a).filter(|a| run_computer(*a, ins, false) == ins);
    }

    let last = target.len() - 1;
    let last_ins = target[last];

    for i in 0..8 {
        // if a == 0 && i == 0 {
        //     // This avoid cases where the trailing 0 is missing in the final output
        //     continue;
        // }

        let candidate = (a << 3) + i;
        if run_computer(candidate, ins, true)[0] == last_ins
            && let Some(res) = solve_p2(candidate, &target[0..last], ins)
        {
            return Some(res);
        }
    }

    None
}

#[aoc(day17, part2)]
pub fn part2((_, program): &(IntType, Vec<IntType>)) -> IntType {
    // the program must contain only one ADV, and that must be ADV 3
    assert_eq!(program.chunks(2).filter(|&w| w[0] == 0).count(), 1);
    assert_eq!(program.chunks(2).filter(|&w| w == [0, 3]).count(), 1);

    // the program must contain only one Out
    assert_eq!(program.chunks(2).filter(|&w| w[0] == 5).count(), 1);

    // the program must end with JNZ 0
    assert_eq!(program[program.len() - 2..], [3, 0]);

    // This only works because the previous asserts are true.
    //
    // * The instruction set only has one way of modifying A, via the ADV ins.
    // JNZ and OUT do not modify any register.
    // All other instructions modifying registers modify B or C.
    // So at somepoint we will modify the A register with ADV 3 this shifts A
    // to the right 3
    //
    // * There is only one Out instruction, we only output one number per loop
    //
    // * At the end it will jump to the beginning (JNZ 0)
    //
    // so in psuedo-code all programs must look something like this
    //
    // do {
    //    b = (a % 8) ^ 4; // BST 4; BXL 4
    //    c = a >> b;      // CDV 5
    //    b = b ^ c ^ 4;   // BXC 1; BXL 4
    //    output(b % 8);   // OUT 5
    //    a = a >> 3;      // ADV 3
    // } while a != 0;     // JNZ 0
    //
    // We modify B and C, output, shift A right, and then loop. The ordering
    // isn't important, except that JNZ is at the end
    //
    // Each time through we calculate B and C based on A and only A.
    //
    // To solve this, we will loop through the input program backwards.
    // We will pick an A (0 to 7), such that it produces the right program digit.
    // Then we left shift A by 3, and check every variation of those 3 new bits.
    // As there are possibly 0 or more matching at each step, we use recursion
    // to allow us to backtrack.
    solve_p2(0, program, program).unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r"Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0";

    #[test]
    pub fn input_test() {
        println!("{:?}", generator(SAMPLE));

        // assert_eq!(generator(SAMPLE), Object());
    }

    #[test]
    pub fn part1_test() {
        assert_eq!(part1(&generator(SAMPLE)), "4,6,3,5,6,3,5,2,1,0");
    }

    #[test]
    pub fn part2_test() {
        assert_eq!(part2(&(0, vec![0, 3, 5, 4, 3, 0])), 117440);
        assert_eq!(
            part2(&(0, vec![2, 4, 1, 2, 7, 5, 4, 1, 1, 3, 5, 5, 0, 3, 3, 0])),
            37221261688308,
        );
        assert_eq!(
            part2(&(0, vec![2, 4, 1, 3, 7, 5, 1, 6, 1, 4, 5, 5, 0, 3, 3, 0])),
            45188036846635,
        );
    }

    mod regression {
        use super::*;

        const INPUT: &str = include_str!("../input/2024/day17.txt");
        const ANSWERS: (&str, IntType) = ("7,0,7,3,4,1,3,0,1", 156985331222018);

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            let output = generator(input);

            assert_eq!(part1(&output), ANSWERS.0);
            assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
