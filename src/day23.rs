use ahash::{HashMapExt, HashSetExt};
use aoc_runner_derive::{aoc, aoc_generator};
use itertools::Itertools;
use petgraph::graph::{NodeIndex, UnGraph};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::common::parse::parse_split_once;

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct SStr([u8; 2]);

impl std::str::FromStr for SStr {
    type Err = std::array::TryFromSliceError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.as_bytes().try_into().map(Self)
    }
}

impl SStr {
    fn as_str(&self) -> &str {
        // SAFETY: node_weight's are all [u8; 2] and ascii
        unsafe { std::str::from_utf8_unchecked(self.0.as_slice()) }
    }

    fn starts_with(&self, p: u8) -> bool {
        self.0[0] == p
    }
}

#[aoc_generator(day23)]
pub fn generator(input: &str) -> UnGraph<SStr, ()> {
    let mut graph = UnGraph::new_undirected();
    let mut node_map = HashMap::new();

    for line in input.lines() {
        let (a, b) = parse_split_once(line, '-').unwrap();

        let node1 = *node_map.entry(a).or_insert_with(|| graph.add_node(a));
        let node2 = *node_map.entry(b).or_insert_with(|| graph.add_node(b));
        graph.add_edge(node1, node2, ());
    }

    graph
}

#[aoc(day23, part1)]
pub fn part1(graph: &UnGraph<SStr, ()>) -> usize {
    let mut triangles = HashSet::new();

    for edge in graph.edge_indices() {
        let (a, b) = graph.edge_endpoints(edge).unwrap();
        let neighbors_a: HashSet<_> = graph.neighbors(a).collect();

        for c in graph.neighbors(b) {
            if neighbors_a.contains(&c) {
                let mut triangle = [a, b, c];

                if triangle
                    .iter()
                    .any(|x| graph.node_weight(*x).unwrap().starts_with(b't'))
                {
                    triangle.sort_unstable();
                    triangles.insert(triangle);
                }
            }
        }
    }

    triangles.len()
}

fn bron_kerbosch(
    graph: &UnGraph<SStr, ()>,
    r: &mut HashSet<NodeIndex>,
    mut p: HashSet<NodeIndex>,
    mut x: HashSet<NodeIndex>,
    max_clique: &mut HashSet<NodeIndex>,
) {
    if p.is_empty() && x.is_empty() {
        if r.len() > max_clique.len() {
            max_clique.clear();
            max_clique.extend(r.iter());
        }
        return;
    }

    let pivot = p.union(&x).next().copied().unwrap();
    let pivot_neighbors = graph.neighbors(pivot).collect();

    for v in p.clone().difference(&pivot_neighbors).copied() {
        r.insert(v);

        let neighbors = graph.neighbors(v).collect();
        let p_new = p.intersection(&neighbors).copied().collect();
        let x_new = x.intersection(&neighbors).copied().collect();

        bron_kerbosch(graph, r, p_new, x_new, max_clique);

        r.remove(&v);
        p.remove(&v);
        x.insert(v);
    }
}

#[aoc(day23, part2)]
pub fn part2(graph: &UnGraph<SStr, ()>) -> String {
    // let mut max_clique = std::collections::HashSet::new();

    // pathfinding::undirected::cliques::maximal_cliques(
    //     graph.node_indices(),
    //     &mut |a, b| graph.contains_edge(*a, *b),
    //     &mut |hs| {
    //         if max_clique.len() < hs.len() {
    //             max_clique = hs.clone();
    //         }
    //     },
    // );

    let mut max_clique = HashSet::new();

    bron_kerbosch(
        graph,
        &mut Default::default(),
        graph.node_indices().collect(),
        Default::default(),
        &mut max_clique,
    );

    max_clique
        .into_iter()
        .map(|idx| graph.node_weight(idx).unwrap().as_str())
        .sorted_unstable()
        .join(",")
}

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE: &str = r"kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn";

    #[test]
    pub fn input_test() {
        println!("{:?}", generator(SAMPLE));

        // assert_eq!(generator(SAMPLE), Object());
    }

    #[test]
    pub fn part1_test() {
        assert_eq!(part1(&generator(SAMPLE)), 7);
    }

    #[test]
    pub fn part2_test() {
        assert_eq!(part2(&generator(SAMPLE)), "co,de,ka,ta");
    }

    mod regression {
        use super::*;

        const INPUT: &str = include_str!("../input/2024/day23.txt");
        const ANSWERS: (usize, &str) = (998, "cc,ff,fh,fr,ny,oa,pl,rg,uj,wd,xn,xs,zw");

        #[test]
        pub fn test() {
            let input = INPUT.trim_end_matches('\n');
            let output = generator(input);

            assert_eq!(part1(&output), ANSWERS.0);
            assert_eq!(part2(&output), ANSWERS.1);
        }
    }
}
