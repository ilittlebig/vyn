/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-04
 **/

use std::borrow::Cow;
use std::io::{ self, Write };
use termcolor::{ Color, ColorChoice, ColorSpec, StandardStream, WriteColor };

use crate::source::SourceFile;
use crate::diagnostics::{ self, Span, Severity };

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

    pub fn warning(message: impl Into<Cow<'static, str>>, span: Span) -> Diagnostic {
        Diagnostic { severity: Severity::Warning, message: message.into(), span }
    }
}

pub struct Emitter {
    out: StandardStream,
}

impl Emitter {
    pub fn new(color_choice: ColorChoice) -> Self {
        Self { out: StandardStream::stderr(color_choice) }
    }

    pub fn stderr() -> Self {
        Self { out: StandardStream::stderr(ColorChoice::Auto) }
    }

    pub fn emit(&mut self, source_file: &SourceFile, diagnostic: &Diagnostic) -> io::Result<()> {
        let out = &mut self.out;

        let span = diagnostic.span;
        let (line, col) = source_file.line_col(span.start);
        let (line_text, line_start, line_end) = source_file.line_span(span.start);

        out.set_color(&diagnostics::severity_spec(diagnostic.severity))?;
        write!(out, "{}", diagnostics::severity_label(diagnostic.severity))?;

        out.reset()?;
        writeln!(out, ": {}", diagnostic.message)?;

        let w = (line + 1).to_string().len();
        out.set_color(&diagnostics::gutter_spec())?;
        write!(out, "{:>w$} --> ", "", w = w - 1)?;
        out.reset()?;
        writeln!(out, "{}:{}:{}", source_file.name, line + 1, col + 1)?;

        out.set_color(&diagnostics::gutter_spec())?;
        writeln!(out, "{:>w$} |", "", w = w)?;
        out.reset()?;

        out.set_color(&diagnostics::gutter_spec())?;
        write!(out, "{:>w$} |", line + 1, w = w)?;
        out.reset()?;

        writeln!(out, " {}", line_text)?;

        let highlight_start = span.start.max(line_start);
        let highlight_end = span.end.min(line_end);
        let width = (highlight_end.saturating_sub(highlight_start)).max(1);

        let marker = format!(
            "{}^{}",
            " ".repeat(col),
            "~".repeat(width.saturating_sub(1))
        );

        out.set_color(&diagnostics::gutter_spec())?;
        write!(out, "{:>w$} | ", "", w = w)?;
        out.reset()?;

        out.set_color(&diagnostics::severity_spec(diagnostic.severity))?;
        writeln!(out, "{}", marker)?;
        out.reset()?;
        writeln!(out, "")?;

        Ok(())
    }
}
