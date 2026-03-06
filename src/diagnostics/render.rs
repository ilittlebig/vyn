/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-06
 **/

use termcolor::{ Color, ColorChoice, ColorSpec, StandardStream, WriteColor };

#[derive(Debug, Clone, Copy)]
pub enum Severity { Error, Warning, Note }

pub fn severity_label(severity: Severity) -> &'static str {
    match severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Note => "note",
    }
}

pub fn severity_spec(severity: Severity) -> ColorSpec {
    let mut spec = ColorSpec::new();
    spec.set_bold(true);
    match severity {
        Severity::Error => { spec.set_fg(Some(Color::Red)); }
        Severity::Warning => { spec.set_fg(Some(Color::Yellow)); }
        Severity::Note => { spec.set_fg(Some(Color::Cyan)); }
    }
    spec
}

pub fn gutter_spec() -> ColorSpec {
    let mut spec = ColorSpec::new();
    spec.set_bold(true);
    spec.set_fg(Some(Color::Rgb(0, 0, 245)));
    spec
}
