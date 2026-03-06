/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-04
 **/

#[derive(Debug)]
pub enum CommandKind {
    Run,
    //Build,
    //Check,
    //Help,
}

#[derive(Debug)]
pub enum Mode {
    Debug,
    Release,
}

#[derive(Debug)]
pub enum Target {
    Native
}

#[derive(Debug)]
pub struct CompileOptions {
    pub mode: Mode,
    pub target: Target,
    pub dump_hir: bool,
}

#[derive(Debug)]
pub struct RunArgs {
    pub compile: CompileOptions,
    pub program_args: Vec<String>,
}

#[derive(Debug)]
pub enum CliCommand {
    Run(RunArgs),
    //Build(BuildArgs),
    //Check(CheckArgs),
    //Help
}

impl CliCommand {
    pub fn compile(&self) -> Option<&CompileOptions> {
        match self {
            CliCommand::Run(args) => Some(&args.compile),
            _ => None,
        }
    }
}
