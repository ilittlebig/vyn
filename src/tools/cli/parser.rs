/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-04
 **/

use std::fmt::Debug;
use std::iter::{ Iterator, Peekable };

use crate::tools::cli::{ CliError, CliCommand, CompileOptions, RunArgs, Target, Mode };

struct ParserOptions {
    dump_hir: bool,
}

impl Default for ParserOptions {
    fn default() -> Self {
        Self { dump_hir: false }
    }
}

struct Parser<I> where I: Iterator<Item = String> {
    it: Peekable<I>,
    mode: Mode,
    target: Target,
    options: ParserOptions,
}

impl<I> Parser<I> where I: Iterator<Item = String> {
    fn new(it: I) -> Self {
        Self {
            it: it.peekable(),
            mode: Mode::Debug,
            target: Target::Native,
            options: ParserOptions::default(),
        }
    }

    fn peek(&mut self) -> Option<&String> {
        self.it.peek()
    }

    fn next(&mut self) -> Option<String> {
        self.it.next()
    }

    fn parse_mode(&mut self) {
    }

    fn parse_target(&mut self) -> Result<(), CliError> {
        let token = self.next().ok_or(CliError::MissingValue { flag: "--target" })?;
        match token.as_str() {
            "native" => { Ok(()) },
            _ => Err(CliError::UnexpectedValue { flag: "--target", value: token }),
        }?;
        Ok(())
    }

    fn parse_arg(&mut self) -> Result<(), CliError> {
        let Some(token) = self.next() else { return Ok(()); };
        match token.as_str() {
            "--dump-hir" => self.options.dump_hir = true,
            "--target" => self.parse_target()?,
            _ => println!("unknown"),
        }
        Ok(())
    }

    fn parse_args(&mut self) -> Result<(), CliError> {
        while self.peek().is_some() {
            self.parse_arg()?;
        }
        Ok(())
    }
}

pub fn parse<I: Iterator<Item = String>>(mut it: I) -> CliCommand where I::Item: Debug {
    let mut parser = Parser::new(it);
    if let Err(e) = parser.parse_args() {
        eprintln!("{e}");
    };

    let compile_options = CompileOptions {
        mode: parser.mode,
        target: parser.target,
    };

    CliCommand::Run(RunArgs {
        compile: compile_options,
        args: Vec::new(),
    })
}
