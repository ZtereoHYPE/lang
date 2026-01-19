mod ir_panel;
mod locals_panel;

use crate::interpreter::ir_interpreter::IrInterpreter;
use crate::representations::ir;
use crate::tui::ir_panel::IrPanel;
use crate::tui::locals_panel::LocalsPanel;
use crossterm::event;
use crossterm::event::{Event, KeyCode, KeyEvent};
use ratatui::buffer::Buffer;
use ratatui::layout::{Layout, Rect};
use ratatui::prelude::*;
use ratatui::style::palette::tailwind;
use ratatui::widgets::{Block, BorderType, Padding, Paragraph};

const BORDER_STYLE: Style = Style::new().fg(Color::Rgb(100, 100, 100));
const BLOCK: Block = Block::bordered().border_type(BorderType::Rounded);
const BACKGROUND: Color = tailwind::NEUTRAL.c900;


pub fn run_tui(ir: ir::Program) -> std::io::Result<()> {
    let ir_copy = ir.clone();
    let interpreter = IrInterpreter::new(&ir);
    let mut app = InterpreterTui::new(ir_copy, interpreter);

    ratatui::run(|terminal| {
        while !app.should_quit {
            terminal.draw(|frame| frame.render_widget(&app, frame.area()))?;
            app.handle_event(event::read()?);
        }
        Ok(())
    })
}


struct InterpreterTui<'a> {
    ir: ir::Program,
    interpreter: IrInterpreter<'a>,
    should_quit: bool,
}

impl<'a> InterpreterTui<'a> {
    fn new(ir: ir::Program, interpreter: IrInterpreter<'a>) -> Self {
        InterpreterTui {
            ir,
            interpreter,
            should_quit: false,
        }
    }

    fn handle_event(&mut self, event: Event) {
        match event {
            Event::Key(KeyEvent { code, .. }) => match code {
                KeyCode::Char('q') => self.should_quit = true,
                KeyCode::Char('n') => {
                    if let Some(_) = self.interpreter.state() {
                        self.interpreter.execute_step()
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

impl Widget for &InterpreterTui<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![Constraint::Fill(1), Constraint::Length(2)])
            .split(area);

        let interpreter_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(vec![Constraint::Fill(2), Constraint::Fill(1)])
            .split(layout[0]);

        if let Some(state) = self.interpreter.state() {
            IrPanel::new(self.ir.clone(), state)
                .render(interpreter_layout[0], buf);

            let variables = self.interpreter.state().unwrap().locals.clone();
            LocalsPanel::new(variables)
                .render(interpreter_layout[1], buf);

        } else {
            Paragraph::new(format!("Program terminated! Output: {}", self.interpreter.output.to_string()))
                .centered()
                .white()
                .bold()
                .block(Block::new().padding(Padding::uniform(3)).bg(BACKGROUND))
                .render(layout[0], buf);
        }

        let shortcuts_block = Block::new().padding(Padding::left(1)).bg(BACKGROUND);
        Paragraph::new("q: Quit   n: Next Line")
            .block(shortcuts_block)
            .render(layout[1], buf)
    }
}
