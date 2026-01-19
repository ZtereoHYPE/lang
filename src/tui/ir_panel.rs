use crate::interpreter::ir_interpreter::CallFrame;
use crate::representations::ast::{Literal, Operator};
use crate::representations::ir;
use crate::representations::ir::{Atom, Expression};
use crate::tui::{BACKGROUND, BLOCK, BORDER_STYLE};
use itertools::Itertools;
use ratatui::prelude::Stylize;
use ratatui::prelude::*;
use ratatui::style::palette::tailwind::SLATE;
use ratatui::style::Styled;
use ratatui::widgets::{HighlightSpacing, List, ListState};
use std::cmp::Ordering;
use std::fmt;
use std::fmt::Formatter;


const LABEL_STYLE: Style = Style::new()
    .fg(Color::Rgb(148, 199, 196))
    .bold();

const ASS_STYLE: Style = Style::new()
    .fg(Color::White);

const TERMINAL_STYLE: Style = Style::new()
    .fg(Color::Rgb(232, 199, 116));

pub struct IrPanel {
    pub ir: ir::Program,
    pub function: String,
    pub block: String,
    pub statement: usize,
}

impl Widget for IrPanel {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let function = self.ir.functions
            .into_iter()
            .find(|f| f.name.id == self.function)
            .expect("fn dont exist??");

        let title = Line::from(vec![
            "Function ".white(),
            self.function.blue().bold(),
            " - Block ".white(),
            self.block.clone().blue().bold(),
            " - Statement ".white(),
            self.statement.blue().bold()
        ]).centered();

        let block = BLOCK
            .title(title)
            .bg(BACKGROUND)
            .border_style(BORDER_STYLE);

        let (fn_lines, line_nr) = get_function_lines(function, self.block, self.statement);

        let list = List::new(fn_lines)
            .block(block)
            .highlight_style(Style::new().bg(SLATE.c700).add_modifier(Modifier::BOLD))
            .highlight_symbol(">> ")
            .highlight_spacing(HighlightSpacing::Always)
            .scroll_padding(2000);

        let mut state = ListState::default();
        state.select(Some(line_nr));

        StatefulWidget::render(list, area, buf, &mut state);
    }
}

impl IrPanel {
    pub(crate) fn new(ir: ir::Program, state: &CallFrame) -> Self {
        let function = state.ip.function.name.id.clone();
        let block = state.ip.block.name.clone();
        let statement = state.ip.statement;

        Self { ir, function, block, statement }
    }
}

fn get_function_lines(function: ir::Function, block_name: String, statement: usize) -> (Vec<Line<'static>>, usize) {
    let mut lines = Vec::new();
    let mut line_nr = 0;

    let sorted_blocks = function.blocks
        .into_iter()
        .sorted_by(|(l, _), (r, _)| block_comparator(l, r));

    for (name, block) in sorted_blocks {
        if name == block_name {
            line_nr = lines.len() + statement + 1; // 1 for the header
        }

        lines.push(Line::from(name + ":").style(LABEL_STYLE));
        for (id, exp) in &block.assignments {
            lines.push(
                Line::from(format!("    {} = {}", id.id, exp.to_string())).style(ASS_STYLE)
            )
        }

        lines.push(block.terminal.get_line());
        lines.push(Line::from(""));
    }

    (lines, line_nr)
}

fn block_comparator(left: &String, right: &String) -> Ordering {
    let left_int = left[6..].parse::<usize>();
    let right_int = right[6..].parse::<usize>();

    if let (Ok(l), Ok(r)) = (left_int, right_int) {
        Ord::cmp(&r, &l)
    } else {
        Ord::cmp(left, right)
    }
}

impl ir::Terminal {
    fn get_line(&self) -> Line<'static> {
        match self {
            ir::Terminal::Goto { label } => Line::from(vec![
                "    goto ".set_style(TERMINAL_STYLE),
                label.clone().set_style(LABEL_STYLE)
            ]),

            ir::Terminal::Conditional { condition, then_label, else_label } => Line::from(vec![
                "    if (".set_style(TERMINAL_STYLE),
                condition.to_string().set_style(ASS_STYLE).into(),
                ") ".set_style(TERMINAL_STYLE),
                then_label.clone().set_style(LABEL_STYLE),
                " else ".set_style(TERMINAL_STYLE),
                else_label.clone().set_style(LABEL_STYLE)
            ]),

            ir::Terminal::Return(e) => Line::from(vec![
                "    return ".set_style(TERMINAL_STYLE),
                e.to_string().set_style(ASS_STYLE).into(),
            ])
        }
    }
}

impl fmt::Display for ir::Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Unary { op, atom }      => write!(f, "{}{}", op.to_string(), atom.to_string()),
            Expression::Binary { lhs, op, rhs } => write!(f, "{} {} {}", lhs.to_string(), op.to_string(), rhs.to_string()),
            Expression::FunCall { id, args }    => write!(f, "{}({})", id.id, args.iter().map(|a| a.to_string()).fold(String::new(), |s, a| if s.len() > 0 {s + ", " + &a} else {a})), // folds are my best friend
            Expression::Atom(a)                 => write!(f, "{}", a.to_string())
        }
    }
}

impl fmt::Display for ir::Atom {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Atom::Variable { id }   => write!(f, "{}", id.id),
            Atom::Value(literal) => match literal {
                Literal::Bool(b)    => write!(f, "{}", b.to_string()),
                Literal::Int(i)     => write!(f, "{}", i.to_string()),
                Literal::Unit()     => write!(f, "()")
            }
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Operator::Plus                  => write!(f, "+"),
            Operator::Minus                 => write!(f, "-"),
            Operator::Times                 => write!(f, "*"),
            Operator::Divided               => write!(f, "/"),
            Operator::Negated               => write!(f, "-"),
            Operator::And                   => write!(f, "&"),
            Operator::Or                    => write!(f, "|"),
            Operator::Xor                   => write!(f, "^"),
            Operator::Not                   => write!(f, "!"),
            Operator::GreaterThan           => write!(f, ">"),
            Operator::GreaterOrEqualThan    => write!(f, ">="),
            Operator::LessThan              => write!(f, "<"),
            Operator::LessOrEqualThan       => write!(f, "<="),
            Operator::Equal                 => write!(f, "=="),
            Operator::Different             => write!(f, "!="),
        }
    }
}