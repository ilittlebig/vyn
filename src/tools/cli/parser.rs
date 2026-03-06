/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-04
 **/

use std::fmt::Debug;
use std::iter::{ Iterator, Peekable };

use crate::tools::cli::{ CliError, CliCommand, CompileOptions, RunArgs, Target, Mode, CommandKind };

#[derive(Debug)]
struct ParsedCli {
    command: Option<CommandKind>,
    compile: CompileOptions,
    path: Option<String>,
    program_args: Vec<String>,
}

impl Default for ParsedCli {
    fn default() -> Self {
        Self {
            command: None,
            compile: CompileOptions {
                mode: Mode::Debug,
                target: Target::Native,
                dump_hir: false,
            },
            path: None,
            program_args: Vec::new(),
        }
    }
}

impl ParsedCli {
    fn into_command(self) -> Result<CliCommand, CliError> {
        match self.command {
            Some(CommandKind::Run) => Ok(CliCommand::Run(RunArgs {
                compile: self.compile,
                program_args: self.program_args
            })),
            None => Err(CliError::MissingCommand),
        }
    }
}

struct Parser<I> where I: Iterator<Item = String> {
    it: Peekable<I>,
    out: ParsedCli,
}

impl<I> Parser<I> where I: Iterator<Item = String> {
    fn new(it: I) -> Self {
        Self { it: it.peekable(), out: ParsedCli::default() }
    }

    fn peek(&mut self) -> Option<&String> {
        self.it.peek()
    }

    fn next(&mut self) -> Option<String> {
        self.it.next()
    }

    fn parse_mode(&mut self) -> Result<(), CliError> {
        let token = self.next().ok_or(CliError::MissingValue { flag: "--mode" })?;
        match token.as_str() {
            "debug" => { self.out.compile.mode = Mode::Debug; },
            "release" => { self.out.compile.mode = Mode::Release; },
            _ => Err(CliError::UnexpectedValue { flag: "--mode", value: token })?,
        };
        Ok(())
    }

    fn parse_target(&mut self) -> Result<(), CliError> {
        let token = self.next().ok_or(CliError::MissingValue { flag: "--target" })?;
        match token.as_str() {
            "native" => {},
            _ => Err(CliError::UnexpectedValue { flag: "--target", value: token })?,
        };
        Ok(())
    }

    fn parse_arg(&mut self) -> Result<(), CliError> {
        let Some(token) = self.next() else { return Ok(()); };
        match token.as_str() {
            "--dump-hir" => self.out.compile.dump_hir = true,
            "--mode" => self.parse_mode()?,
            "--target" => self.parse_target()?,
            _ => Err(CliError::UnexpectedFlag { flag: token })?,
        }
        Ok(())
    }

    fn parse_args(mut self) -> Result<ParsedCli, CliError> {
        let Some(command) = self.next() else {
            //self.out.command = Some(CommandKind::Help);
            return Ok(self.out);
        };

        match command.as_str() {
            "run" => self.out.command = Some(CommandKind::Run),
            //"build" => self.out.command = Some(CommandKind::Build),
            //"check" => self.out.command = Some(CommandKind::Check),
            //"help" | "-h" | "--help" => self.out.command = Some(CommandKind::Help),
            _ => return Err(CliError::UnknownCommand { cmd: command }),
        }

        while self.peek().is_some() {
            self.parse_arg()?;
        }
        Ok(self.out)
    }
}

pub fn parse<I: Iterator<Item = String>>(it: I) -> Result<CliCommand, CliError> {
    let mut parser = Parser::new(it);
    let parsed_cli = parser.parse_args()?;
    parsed_cli.into_command()
}
