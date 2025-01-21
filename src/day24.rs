use ahash::{HashMap, HashMapExt};
use aoc_runner_derive::{aoc, aoc_generator};
use itertools::Itertools;

use crate::common::sstr::SStr;

trait Extension {
    fn make_wire(start: u8, number: u8) -> Self;
    fn is_xy(&self) -> bool;
    fn is_xy00(&self) -> bool;
}

impl Extension for SStr<3> {
    fn make_wire(start: u8, number: u8) -> Self {
        let a = number / 10;
        let b = number % 10;

        assert!(a < 10);
        assert!(b < 10);

        Self::from([start, b'0' + a, b'0' + b])
    }

    fn is_xy(&self) -> bool {
        self.starts_with(b'x') || self.starts_with(b'y')
    }

    fn is_xy00(&self) -> bool {
        self.as_str() == "x00" || self.as_str() == "y00"
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum Kind {
    Value(u8),
    And(SStr<3>, SStr<3>),
    Or(SStr<3>, SStr<3>),
    Xor(SStr<3>, SStr<3>),
}

#[aoc_generator(day24)]
pub fn generator(input: &str) -> HashMap<SStr<3>, Kind> {
    let mut hm = HashMap::new();
    let (inputs, gates) = input.split_once("\n\n").unwrap();

    for line in inputs.lines() {
        let (key, value) = line.split_once(": ").unwrap();
        let key = key.parse().unwrap();
        let value = if value == "1" { 1 } else { 0 };

        hm.insert(key, Kind::Value(value));
    }

    for line in gates.lines() {
        let mut token = line.split(' ');
        let b1: SStr<3> = token.next().unwrap().parse().unwrap();
        let op = token.next().unwrap();
        let b2: SStr<3> = token.next().unwrap().parse().unwrap();
        let _ = token.next().unwrap();
        let b3 = token.next().unwrap().parse().unwrap();

        let (b1, b2) = if b1.starts_with(b'y') && b2.starts_with(b'x') {
            (b2, b1)
        } else {
            (b1, b2)
        };

        let kind = match op {
            "XOR" => Kind::Xor(b1, b2),
            "OR" => Kind::Or(b1, b2),
            "AND" => Kind::And(b1, b2),
            _ => panic!("unknown op"),
        };

        hm.insert(b3, kind);
    }

    hm
}

fn solve_p1(inputs: &HashMap<SStr<3>, Kind>, cache: &mut HashMap<SStr<3>, u8>) {
    fn recurse(
        key: SStr<3>,
        inputs: &HashMap<SStr<3>, Kind>,
        cache: &mut HashMap<SStr<3>, u8>,
    ) -> u8 {
        if let Some(v) = cache.get(&key).copied() {
            return v;
        }

        if let Some(rule) = inputs.get(&key) {
            let value = match rule {
                Kind::Value(v) => *v,
                Kind::And(r1, r2) => {
                    let r1 = recurse(*r1, inputs, cache);
                    let r2 = recurse(*r2, inputs, cache);
                    r1 & r2
                }
                Kind::Or(r1, r2) => {
                    let r1 = recurse(*r1, inputs, cache);
                    let r2 = recurse(*r2, inputs, cache);
                    r1 | r2
                }
                Kind::Xor(r1, r2) => {
                    let r1 = recurse(*r1, inputs, cache);
                    let r2 = recurse(*r2, inputs, cache);
                    r1 ^ r2
                }
            };

            cache.insert(key, value);
            value
        } else {
            panic!("unknown key")
        }
    }

    for k in inputs.keys().filter(|k| k.starts_with(b'z')) {
        recurse(*k, inputs, cache);
    }
}

#[aoc(day24, part1)]
pub fn part1(inputs: &HashMap<SStr<3>, Kind>) -> u64 {
    let mut values = Default::default();
    solve_p1(inputs, &mut values);

    values
        .iter()
        .filter(|(k, _)| k.starts_with(b'z'))
        .sorted_by_key(|(k, _)| *k)
        .enumerate()
        .fold(0, |acc, (i, (_, v))| acc | (u64::from(*v) << (i as u64)))
}

#[aoc(day24, part2)]
pub fn part2(inputs: &HashMap<SStr<3>, Kind>) -> String {
    // When visualizing the input, you will see that it is a full-adder
    // with xNN and yNN as the input bit, and zNN is the output bit
    //
    // The following operations in one step of a full adder
    // x[n] ^ y[n] => xyADD[n]
    // x[n] & y[n] => xyCARRY[n]
    // xyADD[n] ^ CARRY[n - 1] => z[n]
    // xyADD[n] & CARRY[n - 1] => AND[n]
    // xyCARRY[n] | AND[n] => CARRY[n]

    let mut wrongs: Vec<&SStr<3>> = Vec::with_capacity(8);
    let mut xy_adds = Vec::with_capacity(45);
    let mut xy_carries = Vec::with_capacity(45);
    let mut z: Vec<(SStr<3>, SStr<3>, SStr<3>)> = Vec::with_capacity(45);
    let mut ands = Vec::with_capacity(45);
    let mut carries = Vec::with_capacity(45);

    for rule in inputs.iter() {
        let rule = (*rule.0, *rule.1);
        match rule.1 {
            Kind::Value(_) => {}
            Kind::Xor(a, b) => {
                if a.is_xy() || b.is_xy() {
                    // The XOR that include x or y
                    xy_adds.push((rule.0, a, b));
                } else {
                    // Otherwise it's a Z
                    z.push((rule.0, a, b));
                }
            }
            Kind::And(a, b) => {
                if a.is_xy() || b.is_xy() {
                    // The AND's that include x or y
                    xy_carries.push((rule.0, a, b));
                } else {
                    // Otherwise it's the remaining and
                    ands.push((rule.0, a, b));
                }
            }
            Kind::Or(a, b) => {
                if a.is_xy() || b.is_xy() {
                    panic!("this shouldn't happen")
                } else {
                    // How carries are created
                    carries.push((rule.0, a, b));
                }
            }
        }
    }

    // if xyADD doesn't show up in other XOR operation (or the register is Z), then it is wrong
    wrongs.extend(
        xy_adds
            .iter()
            .filter(|(_, a, _)| !a.is_xy00())
            .filter(|(dest, _, _)| {
                dest.starts_with(b'z') || z.iter().all(|(_, za, zb)| za != dest && zb != dest)
            })
            .map(|(dest, _, _)| dest),
    );

    // if xyCarry doesn't show up in carries (or the register is Z), then it is wrong
    wrongs.extend(
        xy_carries
            .iter()
            .filter(|(_, a, _)| !a.is_xy00())
            .filter(|(dest, _, _)| {
                dest.starts_with(b'z') || carries.iter().all(|(_, ca, cb)| ca != dest && cb != dest)
            })
            .map(|(dest, _, _)| dest),
    );

    // if the z output doesn't start with a z, then it is wrong
    wrongs.extend(
        z.iter()
            .filter(|(dest, _, _)| !dest.starts_with(b'z'))
            .map(|(dest, _, _)| dest),
    );

    // if the carries is a z-register (except for the last z, which is a carry), then it is wrong
    wrongs.extend(
        carries
            .iter()
            .filter(|(dest, _, _)| {
                dest.starts_with(b'z') && *dest != SStr::make_wire(b'z', (z.len() + 1) as u8)
            })
            .map(|(dest, _, _)| dest),
    );

    // if the and output is a z-register then it is wrong
    wrongs.extend(
        ands.iter()
            .filter(|(dest, _, _)| dest.starts_with(b'z'))
            .map(|(dest, _, _)| dest),
    );

    // We're supposed to have 8
    assert_eq!(wrongs.len(), 8);

    wrongs.into_iter().map(|s| s.as_str()).sorted().join(",")
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_PARSE: &str = r"x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02";

    const SAMPLE: &str = r"x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj";

    #[test]
    pub fn input_test() {
        println!("{:?}", generator(SAMPLE_PARSE));

        // assert_eq!(generator(SAMPLE), Object());
    }

    #[test]
    pub fn part1_test() {
        assert_eq!(part1(&generator(SAMPLE)), 2024);
    }

    #[test]
    pub fn part2_test() {
        // assert_eq!(part2(&generator(SAMPLE)), 336);
    }

    mod regression {
        use super::*;

        const INPUT: &str = include_str!("../input/2024/day24.txt");
        const ANSWERS: (u64, &str) = (57632654722854, "ckj,dbp,fdv,kdf,rpp,z15,z23,z39");

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            let output = generator(input);

            assert_eq!(part1(&output), ANSWERS.0);
            assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
