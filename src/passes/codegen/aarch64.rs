/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-09
 **/

use crate::passes::mir::MirProgram;

pub struct Codegen {
    pub mir: MirProgram
}

impl Codegen {
    pub fn emit(&self) {
        println!("LOOL");
    }
}
