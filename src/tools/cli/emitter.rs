/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-06
 **/

use std::borrow::Cow;
use std::io::{ self, Write };
use termcolor::{ Color, ColorChoice, ColorSpec, StandardStream, WriteColor };

#[derive(Debug, Clone, Copy)]
enum CliSeverity { Error }

#[derive(Debug)]
pub struct CliDiagnostic {
    pub severity: CliSeverity,
    pub message: Cow<'static, str>
}

impl CliDiagnostic {
    pub fn error(message: impl Into<Cow<'static, str>>) -> CliDiagnostic {
        CliDiagnostic { severity: CliSeverity::Error, message: message.into() }
    }
}

pub fn print_error(error: CliDiagnostic) -> io::Result<()> {
    let mut out = StandardStream::stderr(ColorChoice::Auto);
    out.set_color(&severity_spec(error.severity))?;
    write!(out, "{}", match error.severity {
        CliSeverity::Error => "error",
    })?;
    out.reset()?;
    writeln!(out, ": {}", error.message)?;
    Ok(())
}

// maybe make some util funcs for these so the diagnostics file
// also wont have to have the exact same code
fn severity_spec(severity: CliSeverity) -> ColorSpec {
    let mut spec = ColorSpec::new();
    spec.set_bold(true);
    match severity {
        CliSeverity::Error => { spec.set_fg(Some(Color::Red)); }
    }
    spec
}
