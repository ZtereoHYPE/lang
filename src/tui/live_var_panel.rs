use crate::states::ast::Identifier;
use crate::tui::{BACKGROUND, BLOCK, BORDER_STYLE};
use itertools::Itertools;
use ratatui::prelude::*;
use ratatui::style::palette::tailwind;
use ratatui::widgets::{Cell, Row, Table};
use std::collections::HashSet;

pub struct LiveVarPanel {
    live_variables: HashSet<Identifier>,
}

impl Widget for LiveVarPanel {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let title = Line::from(vec![
            "Live Variables - ".white(),
            self.live_variables.len().to_string().bold().blue().into(),
            " Entries".bold().white(),
        ])
        .centered();

        let block = BLOCK.title(title).bg(BACKGROUND).border_style(BORDER_STYLE);

        let header = ["#", "Variable"]
            .into_iter()
            .map(Cell::from)
            .collect::<Row>()
            .bold()
            .white()
            .height(1);

        let rows = self
            .live_variables
            .iter()
            .sorted_by_key(|id| &id.id)
            .enumerate()
            .map(|(idx, id)| {
                let color = match idx % 2 {
                    0 => tailwind::NEUTRAL.c800,
                    _ => tailwind::NEUTRAL.c900,
                };
                Row::new(vec![
                    Cell::from(Text::from(idx.to_string())),
                    Cell::from(Text::from(id.id.clone())),
                ])
                .bg(color)
            });

        let table = Table::new(rows, [Constraint::Length(3), Constraint::Fill(1)])
            .header(header)
            .block(block);

        Widget::render(table, area, buf);
    }
}

impl LiveVarPanel {
    pub fn new(live_variables: HashSet<Identifier>) -> Self {
        Self { live_variables }
    }
}
