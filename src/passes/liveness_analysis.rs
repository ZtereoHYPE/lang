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

        Terminal::Conditional { then_label, else_label, condition } => {
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

            let condition = identifiers(&Expression::Atom(condition.clone()));

            before_then
                .union(&before_else)
                .cloned()
                .chain(condition.into_iter())
                .collect()
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

#[cfg(test)]
mod tests {
    use crate::states::ast::{Literal, Operator};
    use super::*;

    fn id(name: &str) -> Identifier {
        Identifier { id: name.to_string() }
    }

    fn dummy_block(name: &str, live_before: HashSet<Identifier>) -> Block {
        Block {
            name: name.to_string(),
            liveness: vec![live_before],
            assignments: vec![],
            terminal: Terminal::Return(Expression::Atom(Atom::Value(Literal::Unit())))
        }
    }

    #[test]
    fn test_identifiers_1() {
        let expr1 = Expression::Atom(Atom::Variable { id: id("x") });

        let expr2 = Expression::FunCall {
            id: id("foo"),
            args: vec![
                Atom::Variable { id: id("y") },
                Atom::Variable { id: id("z") },
            ],
        };

        let ids1 = identifiers(&expr1);
        assert_eq!(ids1, vec![id("x")]);

        let ids2 = identifiers(&expr2);
        assert_eq!(ids2, vec![id("y"), id("z")]);
    }

    #[test]
    fn test_identifiers_2() {
        let expr = Expression::Binary {
            lhs: Atom::Variable { id: id("a") },
            op: Operator::Plus,
            rhs: Atom::Value(Literal::Int(10))
        };

        let ids = identifiers(&expr);
        assert_eq!(ids, vec![id("a")]);

        let expr = Expression::Binary {
            lhs: Atom::Variable { id: id("b") },
            op: Operator::Minus,
            rhs: Atom::Variable { id: id("10") }
        };

        let ids = identifiers(&expr);
        assert_eq!(ids, vec![id("b"), id("10")]);
    }

    #[test]
    fn test_live_before() {
        let assignment = (
            id("a"),
            Expression::Binary {
                lhs: Atom::Variable { id: id("b") },
                op: Operator::Plus,
                rhs: Atom::Value(Literal::Int(5))
            }
        );

        let live = VecDeque::from([HashSet::from([id("a"), id("c")])]);

        let updated_live = live_before(live, &assignment);

        let expected_live: HashSet<Identifier> = HashSet::from([id("b"), id("c")]);

        assert_eq!(updated_live.front().unwrap(), &expected_live);
    }

    #[test]
    fn test_block_liveness_simple() {
        let block = Block {
            name: "b1".into(),
            liveness: vec![],
            assignments: vec![(
                id("x"),
                Expression::Binary {
                    lhs: Atom::Variable { id: id("y") },
                    op: Operator::Plus,
                    rhs: Atom::Variable { id: id("z") }
                }
            )],
            terminal: Terminal::Return(Expression::Atom(Atom::Value(Literal::Unit())))
        };

        let liveness = block_liveness(&block, &HashMap::new());
        assert_eq!(liveness.len(), block.assignments.len() + 1);
        assert_eq!(liveness[0], HashSet::from([id("y"), id("z")]));
        assert_eq!(liveness[1], HashSet::new());
    }

    #[test]
    fn test_block_liveness_overwrite() {
        let mut blocks = HashMap::new();
        blocks.insert("tgt".to_string(), dummy_block("tgt", HashSet::from([id("tmp")])));

        let block = Block {
            name: "b1".into(),
            liveness: vec![],
            assignments: vec![(
                id("tmp"),
                Expression::Binary {
                    lhs: Atom::Variable { id: id("y") },
                    op: Operator::Plus,
                    rhs: Atom::Variable { id: id("z") }
                }
            )],
            terminal: Terminal::Goto { label: "tgt".into() }
        };

        let liveness = block_liveness(&block, &blocks);
        assert_eq!(liveness.len(), block.assignments.len() + 1);
        assert_eq!(liveness[0], HashSet::from([id("y"), id("z")]));
        assert_eq!(liveness[1], HashSet::from([id("tmp")]));
    }

    #[test]
    fn test_block_liveness_multiple() {
        let mut blocks = HashMap::new();
        blocks.insert("then".into(), dummy_block("then", HashSet::from([id("a")])));
        blocks.insert("else".into(), dummy_block("else", HashSet::from([id("b"), id("c")])));

        let block = Block {
            name: "b1".into(),
            liveness: vec![],
            assignments: vec![
                (
                    id("x"),
                    Expression::Binary {
                        lhs: Atom::Variable { id: id("y") },
                        op: Operator::Plus,
                        rhs: Atom::Variable { id: id("z") }
                    }
                ),
                (
                    id("b"),
                    Expression::Unary {
                        op: Operator::Minus,
                        atom: Atom::Variable { id: id("w") }
                    }
                )
            ],
            terminal: Terminal::Conditional {
                condition: Atom::Variable { id: id("cond") },
                then_label: "then".into(),
                else_label: "else".into()
            }
        };

        let liveness = block_liveness(&block, &blocks);
        assert_eq!(liveness.len(), block.assignments.len() + 1);
        assert_eq!(liveness[0], HashSet::from([id("a"), id("c"), id("y"), id("z"), id("cond"), id("w")]));
        assert_eq!(liveness[1], HashSet::from([id("a"), id("c"), id("w"), id("cond")]));
        assert_eq!(liveness[2], HashSet::from([id("a"), id("b"), id("c"), id("cond")]));
    }

    #[test]
    fn test_terminal_liveness() {
        // goto should propagate successor live-before
        let mut blocks = HashMap::new();
        blocks.insert("tgt".to_string(), dummy_block("tgt", HashSet::from([id("x")])));
        let live_goto = terminal_liveness(&Terminal::Goto { label: "tgt".into() }, &blocks);
        assert_eq!(live_goto.front().unwrap(), &HashSet::from([id("x")]));

        // conditional should union both successors, as well as include condition variable
        blocks.insert("then".into(), dummy_block("then", HashSet::from([id("a")])));
        blocks.insert("else".into(), dummy_block("else", HashSet::from([id("b"), id("c")])));
        let live_cond = terminal_liveness(
            &Terminal::Conditional {
                condition: Atom::Variable { id: id("cond") },
                then_label: "then".into(),
                else_label: "else".into()
            },
            &blocks
        );
        assert_eq!(live_cond.front().unwrap(), &HashSet::from([id("a"), id("b"), id("c"), id("cond")]));

        // return has empty live-before
        let live_ret = terminal_liveness(
            &Terminal::Return(Expression::Atom(Atom::Value(Literal::Int(1)))),
            &blocks
        );
        assert_eq!(live_ret.front().unwrap(), &HashSet::new());
    }

    #[test]
    fn test_build_graph() {
        let mut blocks = HashMap::new();
        blocks.insert("b1".into(), Block {
            name: "b1".into(),
            liveness: vec![],
            assignments: vec![],
            terminal: Terminal::Goto { label: "b2".into() }
        });
        blocks.insert("b2".into(), Block {
            name: "b2".into(),
            liveness: vec![],
            assignments: vec![],
            terminal: Terminal::Conditional {
                condition: Atom::Variable { id: id("c") },
                then_label: "loop".into(),
                else_label: "mid".into()
            }
        });
        blocks.insert("loop".into(), Block {
            name: "loop".into(),
            liveness: vec![],
            assignments: vec![],
            terminal: Terminal::Goto { label: "b2".into() }
        });
        blocks.insert("mid".into(), Block {
            name: "mid".into(),
            liveness: vec![],
            assignments: vec![],
            terminal: Terminal::Conditional {
                condition: Atom::Variable { id: id("d") },
                then_label: "b3".into(),
                else_label: "b4".into()
            }
        });
        blocks.insert("b3".into(), Block {
            name: "b3".into(),
            liveness: vec![],
            assignments: vec![],
            terminal: Terminal::Return(Expression::Atom(Atom::Value(Literal::Unit())))
        });
        blocks.insert("b4".into(), Block {
            name: "b4".into(),
            liveness: vec![],
            assignments: vec![],
            terminal: Terminal::Return(Expression::Atom(Atom::Value(Literal::Unit())))
        });

        let func = Function { name: id("f"), params: vec![], entrypoint: "b1".into(), blocks };

        let (graph, returns) = build_graph(&func);

        let mut expected_graph = HashMap::new();
        expected_graph.insert("b2".into(), HashSet::from(["b1".into(), "loop".into()]));
        expected_graph.insert("loop".into(), HashSet::from(["b2".into()]));
        expected_graph.insert("mid".into(), HashSet::from(["b2".into()]));
        expected_graph.insert("b3".into(), HashSet::from(["mid".into()]));
        expected_graph.insert("b4".into(), HashSet::from(["mid".into()]));
        assert_eq!(graph, expected_graph);

        let expected_returns = HashSet::from(["b3".to_string(), "b4".to_string()]);
        assert_eq!(returns.into_iter().collect::<HashSet<_>>(), expected_returns);
    }
}