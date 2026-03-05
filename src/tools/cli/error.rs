/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-05
 **/

use std::fmt;
use std::fmt::Display;

pub enum CliError {
    UnkownCommand { cmd: String },
    MissingValue { flag: &'static str },
    UnexpectedValue { flag: &'static str, value: String },
}

impl Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CliError::UnkownCommand { cmd } => write!(f, "unknown command"),
            CliError::MissingValue { flag } => write!(f, "missing value"),
            CliError::UnexpectedValue { flag, value } => write!(f, "unexpected value for {flag}: {value}"),
        }
    }
}
