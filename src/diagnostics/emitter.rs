/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-04
 **/

use std::borrow::Cow;
use std::io::{ self, Write };
use termcolor::{ Color, ColorChoice, ColorSpec, StandardStream, WriteColor };

use crate::diagnostics::Span;
use crate::source::SourceFile;

#[derive(Debug, Clone, Copy)]
pub enum Severity { Error, Warning, Note }

#[derive(Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: Cow<'static, str>,
    pub span: Span
}

impl Diagnostic {
    pub fn error(message: impl Into<Cow<'static, str>>, span: Span) -> Diagnostic {
        Diagnostic { severity: Severity::Error, message: message.into(), span }
    }
}

fn severity_spec(severity: Severity) -> ColorSpec {
    let mut spec = ColorSpec::new();
    spec.set_bold(true);
    match severity {
        Severity::Error => { spec.set_fg(Some(Color::Red)); }
        Severity::Warning => { spec.set_fg(Some(Color::Yellow)); }
        Severity::Note => { spec.set_fg(Some(Color::Cyan)); }
    }
    spec
}

fn gutter_spec() -> ColorSpec {
    let mut spec = ColorSpec::new();
    spec.set_bold(true);
    spec.set_fg(Some(Color::Rgb(0, 0, 245)));
    spec
}

pub fn print_diagnostic(source_file: &SourceFile, diagnostic: &Diagnostic) -> io::Result<()> {
    let mut out = StandardStream::stderr(ColorChoice::Auto);

    let span = diagnostic.span;
    let (line, col) = source_file.line_col(span.start);
    let (line_text, line_start, line_end) = source_file.line_span(span.start);

    let w = (line + 1).to_string().len();
    write!(&mut out, "{}:{}:{}: ", source_file.name, line + 1, col + 1)?;

    out.set_color(&severity_spec(diagnostic.severity))?;
    write!(
        &mut out,
        "{}",
        match diagnostic.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Note => "note",
        }
    )?;

    out.reset()?;
    writeln!(&mut out, ": {}", diagnostic.message)?;

    out.set_color(&gutter_spec())?;
    write!(&mut out, "{:>w$} |", line + 1, w = w)?;
    out.reset()?;

    writeln!(&mut out, " {}", line_text)?;

    let highlight_start = span.start.max(line_start);
    let highlight_end = span.end.min(line_end);
    let width = (highlight_end.saturating_sub(highlight_start)).max(1);

    let marker = format!(
        "{}^{}",
        " ".repeat(col),
        "~".repeat(width.saturating_sub(1))
    );

    out.set_color(&gutter_spec())?;
    write!(&mut out, "{:>w$} | ", "", w = w)?;
    out.reset()?;

    out.set_color(&severity_spec(diagnostic.severity))?;
    writeln!(&mut out, "{}", marker)?;
    out.reset()?;

    Ok(())
}
