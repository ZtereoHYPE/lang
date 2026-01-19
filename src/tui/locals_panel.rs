use crate::interpreter::ir_interpreter::StackVar;
use crate::interpreter::Value;
use crate::tui::{BACKGROUND, BLOCK, BORDER_STYLE};
use ratatui::prelude::*;
use ratatui::style::palette::tailwind;
use ratatui::widgets::{Cell, Row, Table};
use std::fmt;
use std::fmt::Formatter;

pub struct LocalsPanel {
    live_variables: Vec<StackVar>,
}

impl Widget for LocalsPanel {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let title = Line::from(vec![
            "Locals - ".white(),
            self.live_variables.len().to_string().bold().blue().into(),
            " Entries".bold().white(),
        ])
        .centered();

        let block = BLOCK.title(title).bg(BACKGROUND).border_style(BORDER_STYLE);

        let header = ["#", "Variable", "Value"]
            .into_iter()
            .map(Cell::from)
            .collect::<Row>()
            .bold()
            .white()
            .height(1);

        let rows = self
            .live_variables
            .iter()
            .enumerate()
            .map(|(idx, stackvar)| {
                let color = match (idx % 2, stackvar.alive) {
                    (0, true) => tailwind::NEUTRAL.c800,
                    (0, false) => tailwind::RED.c950,
                    (1, true) => tailwind::NEUTRAL.c900,
                    (1, false) => Color::from_u32(0x360707),
                    _ => unreachable!()
                };

                Row::new(vec![
                    Cell::from(Text::from(idx.to_string())),
                    Cell::from(Text::from(stackvar.id.id.clone())),
                    Cell::from(Text::from(stackvar.value.to_string())),
                ])
                .bg(color)
            });

        let table = Table::new(rows, [Constraint::Length(3), Constraint::Fill(1), Constraint::Fill(1)])
            .header(header)
            .block(block);

        Widget::render(table, area, buf);
    }
}

impl LocalsPanel {
    pub fn new(live_variables: Vec<StackVar>) -> Self {
        Self { live_variables }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b)    => write!(f, "{}", b.to_string()),
            Value::Int(i)     => write!(f, "{}", i.to_string()),
            Value::Unit       => write!(f, "()")
        }
    }
}
