use std::collections::HashMap;
use itertools::Itertools;
use ratatui::prelude::*;
use ratatui::style::palette::tailwind;
use ratatui::widgets::{Block, BorderType, Borders, Cell, HighlightSpacing, List, ListState, Padding, Row, Table};
use crate::tui::{BACKGROUND, BLOCK, BORDER_STYLE};

pub struct LiveVarPanel {
    live_variables: HashMap<String, i32>, // todo: this is just a placeholder
}

impl Widget for LiveVarPanel {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let title = Line::from(vec![
            "Stored Vars - ".white(),
            self.live_variables.len().bold().blue(),
            " Entries".bold().white()
        ]).centered();

        let block = BLOCK
            .title(title)
            .bg(BACKGROUND)
            .border_style(BORDER_STYLE);

        let header = ["#", "Name", "Value"]
            .into_iter()
            .map(Cell::from)
            .collect::<Row>()
            .bold()
            .white()
            .height(1);

        let rows = self.live_variables
            .iter()
            .sorted()
            .enumerate()
            .map(|(idx, (s, i))| {
                let color = match idx % 2 {
                    0 => tailwind::NEUTRAL.c800,
                    _ => tailwind::NEUTRAL.c900
                };
                Row::new(vec![
                    Cell::from(Text::from(idx.to_string())),
                    Cell::from(Text::from(s.clone())),
                    Cell::from(Text::from(i.to_string())),
                ]).bg(color)
            });

        let table = Table::new(rows, [Constraint::Length(3), Constraint::Fill(1), Constraint::Fill(1)])
            .header(header)
            .block(block);

        Widget::render(table, area, buf);
    }
}

impl LiveVarPanel {
    pub(crate) fn new(live_variables: HashMap<String, i32>) -> Self {
        Self { live_variables }
    }
}