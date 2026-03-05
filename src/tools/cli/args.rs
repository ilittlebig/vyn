/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-04
 **/

pub enum Mode {
    Debug,
    Release,
}

pub enum Target {
    Native
}

pub struct CompileOptions {
    pub mode: Mode,
    pub target: Target,
}

pub struct RunArgs {
    pub compile: CompileOptions,
    pub args: Vec<String>,
}

pub enum CliCommand {
    Run(RunArgs),
    //Build(BuildArgs),
    //Check(CheckArgs),
}
