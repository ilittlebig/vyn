/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-06
 **/

use std::borrow::Cow;
use std::io::{ self, Write };
use termcolor::{ Color, ColorChoice, ColorSpec, StandardStream, WriteColor };

use crate::diagnostics::{ self, Severity };

#[derive(Debug)]
pub struct CliDiagnostic {
    pub severity: Severity,
    pub message: Cow<'static, str>
}

impl CliDiagnostic {
    pub fn error(message: impl Into<Cow<'static, str>>) -> CliDiagnostic {
        CliDiagnostic { severity: Severity::Error, message: message.into() }
    }
}

pub fn print_error(error: CliDiagnostic) -> io::Result<()> {
    let mut out = StandardStream::stderr(ColorChoice::Auto);
    out.set_color(&diagnostics::severity_spec(error.severity))?;
    write!(out, "{}", diagnostics::severity_label(error.severity))?;
    out.reset()?;
    writeln!(out, ": {}", error.message)?;
    Ok(())
}
