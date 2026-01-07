use std::collections::{HashMap, HashSet, VecDeque};
use crate::states::ast::Identifier;
use crate::states::ir::{Assignment, Atom, Block, Expression, Function, Program, Terminal};

type Graph = HashMap<String, HashSet<String>>;

pub fn analyze_liveness(p: &mut Program) {
    for function in &mut p.functions {
        let (graph, starts) = build_graph(function);

        // For each starting point, recurse through the blocks
        for start in starts {
            traverse_graph(&graph, &start, &mut function.blocks)
        }
    }
}

fn build_graph(f: &Function) -> (Graph, Vec<String>) {
    let mut graph: Graph = HashMap::new();
    let mut return_blocks: Vec<String> = Vec::new();

    for (_, b) in &f.blocks {
        match &b.terminal {
            Terminal::Goto { label } => {
            graph
                .entry(label.clone())
                .or_insert(HashSet::new())
                .insert(b.name.clone());
            },

            Terminal::Conditional { then_label, else_label, .. } => {
                graph
                    .entry(then_label.clone())
                    .or_insert(HashSet::new())
                    .insert(b.name.clone());

                graph
                    .entry(else_label.clone())
                    .or_insert(HashSet::new())
                    .insert(b.name.clone());
            }

            Terminal::Return(_) => return_blocks.push(b.name.clone())
        }
    }

    (graph, return_blocks)
}

fn traverse_graph(graph: &Graph, start: &String, blocks: &mut HashMap<String, Block>) {
    let old_live_before = blocks[start].get_live_before();

    blocks
        .get_mut(start)
        .unwrap()
        .liveness = block_liveness(&blocks[start], blocks);

    let new_live_before = blocks[start].get_live_before();

    assert!(new_live_before.is_some(), "block_liveness did not generate liveness data!");

    // Keep looping going thru the graph while we change the previous liveness (starts at None)
    if old_live_before != new_live_before {
        for node in graph.get(start).into_iter().flatten() {
            traverse_graph(graph, node, blocks)
        }
    }
}

/// Returns the liveness information of a block
fn block_liveness(b: &Block, blocks: &HashMap<String, Block>) -> Vec<HashSet<Identifier>> {
    b.assignments
        .iter()
        .rev()
        .fold(terminal_liveness(&b.terminal, blocks), live_before)
        .into_iter()
        .collect() // convert into Vec
}

fn terminal_liveness(terminal: &Terminal, blocks: &HashMap<String, Block>) -> VecDeque<HashSet<Identifier>> {
    let live_before = match terminal {
        Terminal::Goto { label } => {
            blocks
                .get(label)
                .unwrap()
                .get_live_before()
                .unwrap_or(HashSet::new())
        }

        Terminal::Conditional { then_label, else_label, .. } => {
            let before_then = blocks
                .get(then_label)
                .unwrap()
                .get_live_before()
                .unwrap_or(HashSet::new());

            let before_else = blocks
                .get(else_label)
                .unwrap()
                .get_live_before()
                .unwrap_or(HashSet::new());

            before_then.union(&before_else).cloned().collect()
        }

        Terminal::Return(_) => HashSet::new()
    };

    VecDeque::from([live_before])
}

fn live_before(mut live: VecDeque<HashSet<Identifier>>, assignment: &Assignment) -> VecDeque<HashSet<Identifier>> {
    let (lhs, rhs) = assignment;

    let mut live_after = live.front().unwrap().clone();
    live_after.remove(lhs);
    live_after.extend(identifiers(rhs));

    live.push_front(live_after);
    live
}

fn identifiers(expression: &Expression) -> Vec<Identifier> {
    let atoms = match expression.clone() {
        Expression::Unary { atom , ..}      => vec![atom],
        Expression::Binary { lhs, rhs, .. } => vec![lhs, rhs],
        Expression::FunCall { args, .. }    => args,
        Expression::Atom(a)                 => vec![a]
    };

    atoms
        .into_iter()
        .filter_map(|a| match a {
            Atom::Variable {id} => Some(id),
            Atom::Value(_)      => None
        })
        .collect()
}

impl Block {
    fn get_live_before(&self) -> Option<HashSet<Identifier>> {
        self.liveness.get(0).map(|l| l.clone())
    }
}
