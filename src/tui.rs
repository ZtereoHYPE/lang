mod ir_panel;
mod live_var_panel;

use std::collections::HashMap;
use crossterm::event;
use crossterm::event::{Event, KeyCode, KeyEvent};
use ratatui::buffer::Buffer;
use ratatui::layout::{Layout, Rect};
use ratatui::prelude::*;
use ratatui::style::palette::tailwind;
use ratatui::widgets::{Block, BorderType, Borders, HighlightSpacing, List, ListState, Padding, Paragraph};
use crate::states::ir;
use crate::tui::ir_panel::IrPanel;
use crate::tui::live_var_panel::LiveVarPanel;

const BORDER_STYLE: Style = Style::new()
    .fg(Color::Rgb(100, 100, 100));

const BLOCK: Block = Block::bordered()
    .border_type(BorderType::Rounded);

const BACKGROUND: Color = tailwind::NEUTRAL.c900;

struct App {
    ir: ir::Program,
    tmp_line_nr: u32,
    should_quit: bool
}

pub fn run_tui(ir: ir::Program) -> std::io::Result<()> {
    let mut app = App { ir, tmp_line_nr: 0, should_quit: false };

    ratatui::run(|terminal| {
        while !app.should_quit {
            terminal.draw(|frame| frame.render_widget(&app, frame.area()))?;
            app.handle_event(event::read()?);
        };
        Ok(())
    })
}


impl Widget for &App {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![
                Constraint::Fill(1),
                Constraint::Length(2)
            ])
            .split(area);

        let interpreter_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(vec![
                Constraint::Fill(2),
                Constraint::Fill(1)
            ])
            .split(layout[0]);

        IrPanel::new(self.ir.clone(), "tester".to_string(), self.tmp_line_nr as usize)
            .render(interpreter_layout[0], buf);

        LiveVarPanel::new(HashMap::from([
            ("x_0".to_string(), 12),
            ("tmp".to_string(), 0),
            ("tmp1".to_string(), 4),
            ("tmp2".to_string(), 1),
            ("tmp3".to_string(), 32427863),
        ]))
            .render(interpreter_layout[1], buf);

        let shortcuts_block = Block::new()
            .padding(Padding::left(1))
            .bg(BACKGROUND);

        Paragraph::new("q: Quit   n: Next Line   ")
            .block(shortcuts_block)
            .render(layout[1], buf)
    }
}

impl App {
    fn handle_event(&mut self, event: Event) {
        match event {
            Event::Key(KeyEvent{ code, .. }) => match code {
                KeyCode::Char('q') => self.should_quit = true,
                KeyCode::Char('n') => self.tmp_line_nr += 1,
                _ => {}
            }
            Event::Mouse(_) => {}
            Event::Resize(_, _) => {}
            _ => {}
        }
    }
}
