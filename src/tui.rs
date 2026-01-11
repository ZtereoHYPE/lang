mod ir_panel;
mod live_var_panel;

use crate::states::ast::Identifier;
use crate::states::ir;
use crate::tui::ir_panel::IrPanel;
use crate::tui::live_var_panel::LiveVarPanel;
use crossterm::event;
use crossterm::event::{Event, KeyCode, KeyEvent};
use ratatui::buffer::Buffer;
use ratatui::layout::{Layout, Rect};
use ratatui::prelude::*;
use ratatui::style::palette::tailwind;
use ratatui::widgets::{Block, BorderType, Padding, Paragraph};
use std::collections::HashSet;

const BORDER_STYLE: Style = Style::new().fg(Color::Rgb(100, 100, 100));

const BLOCK: Block = Block::bordered().border_type(BorderType::Rounded);

const BACKGROUND: Color = tailwind::NEUTRAL.c900;

struct App {
    ir: ir::Program,
    function_name: String,
    line_nr: usize,
    line_liveness_map: Vec<HashSet<Identifier>>,
    max_lines: usize,
    should_quit: bool,
}

impl App {
    fn new(ir: ir::Program, function_name: String) -> Self {
        let (line_liveness_map, max_lines) = Self::build_line_liveness_map(&ir, &function_name);
        App {
            ir,
            function_name,
            line_nr: 0,
            line_liveness_map,
            max_lines,
            should_quit: false,
        }
    }

    /// Build a mapping from line number to the liveness set at that point
    fn build_line_liveness_map(
        ir: &ir::Program,
        function_name: &str,
    ) -> (Vec<HashSet<Identifier>>, usize) {
        let mut liveness_map = Vec::new();

        let function = ir.functions.iter().find(|f| f.name.id == *function_name);

        if let Some(function) = function {
            for (_, block) in &function.blocks {
                // Block label line - show liveness before first instruction
                let live_before = block.liveness.first().cloned().unwrap_or_default();
                liveness_map.push(live_before);

                // Assignment lines - show liveness at that point
                for (i, _) in block.assignments.iter().enumerate() {
                    let live = block.liveness.get(i).cloned().unwrap_or_default();
                    liveness_map.push(live);
                }

                // Terminal line - show liveness after last assignment (before terminal)
                let live_at_terminal = block.liveness.last().cloned().unwrap_or_default();
                liveness_map.push(live_at_terminal);

                // Empty line - no liveness
                liveness_map.push(HashSet::new());
            }
        }

        let max_lines = if liveness_map.is_empty() {
            0
        } else {
            liveness_map.len() - 1
        };
        (liveness_map, max_lines)
    }

    fn get_current_liveness(&self) -> HashSet<Identifier> {
        self.line_liveness_map
            .get(self.line_nr)
            .cloned()
            .unwrap_or_default()
    }

    fn handle_event(&mut self, event: Event) {
        match event {
            Event::Key(KeyEvent { code, .. }) => match code {
                KeyCode::Char('q') => self.should_quit = true,
                KeyCode::Char('n') => {
                    if self.line_nr < self.max_lines {
                        self.line_nr += 1;
                    }
                }
                KeyCode::Char('p') => {
                    if self.line_nr > 0 {
                        self.line_nr -= 1;
                    }
                }
                _ => {}
            },
            Event::Mouse(_) => {}
            Event::Resize(_, _) => {}
            _ => {}
        }
    }
}

pub fn run_tui(ir: ir::Program) -> std::io::Result<()> {
    let mut app = App::new(ir, "tester".to_string());

    ratatui::run(|terminal| {
        while !app.should_quit {
            terminal.draw(|frame| frame.render_widget(&app, frame.area()))?;
            app.handle_event(event::read()?);
        }
        Ok(())
    })
}

impl Widget for &App {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Fill(1), Constraint::Length(2)])
            .split(area);

        let interpreter_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(vec![Constraint::Fill(2), Constraint::Fill(1)])
            .split(layout[0]);

        IrPanel::new(self.ir.clone(), self.function_name.clone(), self.line_nr)
            .render(interpreter_layout[0], buf);

        LiveVarPanel::new(self.get_current_liveness()).render(interpreter_layout[1], buf);

        let shortcuts_block = Block::new().padding(Padding::left(1)).bg(BACKGROUND);

        Paragraph::new("q: Quit   n: Next Line   p: Previous Line")
            .block(shortcuts_block)
            .render(layout[1], buf)
    }
}
