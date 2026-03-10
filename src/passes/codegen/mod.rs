/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-09
 **/

mod aarch64;
mod layout;

use crate::passes::mir::MirProgram;
use aarch64::Codegen;

pub fn emit_program(mir: MirProgram) {
    let codegen = Codegen {
        mir,
    };
    codegen.emit();
}
