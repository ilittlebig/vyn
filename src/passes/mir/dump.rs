/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-03
 **/

use std::fmt::{ self, Write };

use crate::passes::PassContext;
use crate::passes::mir::{
    MirProgram, MirFunction, MirStmt, MirTerm, BasicBlock,
    MirValue,
};

pub struct MirPrinter<'a> {
    pub ctx: &'a PassContext,
    pub out: String,
    pub indent: usize,
}

impl MirPrinter<'_> {
    fn indent(&mut self) {
        let n = self.indent * 4;
        for i in 0..n {
            self.out.push(' ');
        }
    }

    fn with_indent<F: FnOnce(&mut Self)>(&mut self, f: F) {
        self.indent += 1;
        f(self);
        self.indent -= 1;
    }

    fn line(&mut self, s: &str) {
        self.indent();
        let _ = writeln!(&mut self.out, "{s}");
    }

    fn line_fmt(&mut self, args: fmt::Arguments) {
        self.indent();
        let _ = self.out.write_fmt(args);
        self.out.push('\n');
    }

    fn write(&mut self, s: &str) {
        self.indent();
        let _ = write!(&mut self.out, "{s}");
    }

    fn print_program(&mut self, program: &MirProgram) {
        self.line("program {");
        self.with_indent(|p| {
            p.line_fmt(format_args!("entry: fn{}", program.entry.0));
            p.line("");

            for func in &program.funcs {
                p.print_func(&func);
            }
        });
        self.line("}");
    }

    fn print_func(&mut self, func: &MirFunction) {
        let name = if let Some(fn_name) = self.ctx.interner.resolve(func.name) {
            fn_name
        } else {
            // should never happend
            "unknown_symbol"
        };

        self.line_fmt(format_args!("fn{} {}() -> {} {{", func.id.0, name, "nil"));
        self.with_indent(|p| {
            for block in &func.blocks {
                p.print_block(&block);
            }
        });
        self.line("}");
        self.line("");
    }

    fn print_block(&mut self, block: &BasicBlock) {
        self.line_fmt(format_args!("bb{}:", block.id.0));
        self.with_indent(|p| {
            for stmt in &block.stmts {
                p.print_stmt(&stmt);
            }
            p.print_term(&block.term);
        });
    }

    fn print_stmt(&mut self, stmt: &MirStmt) {
        self.line("stmt");
    }

    fn print_term(&mut self, term: &Option<MirTerm>) {
        match term {
            Some(MirTerm::Return(value)) => {
                self.write("return ");
                self.print_value(value);
                self.write("\n");
            },
            Some(MirTerm::Goto(block_id)) => {
                self.line_fmt(format_args!("goto {}", block_id.0));
            },
            Some(MirTerm::If { .. }) => self.line("<unimplemented terminator>"),
            _ => self.line("<missing terminator>"),
        }
    }

    fn print_value(&mut self, value: &MirValue) {
        match value {
            MirValue::ConstInt(i) => {
                let _ = write!(&mut self.out, "const {}", i);
            },
            MirValue::Nil => self.out.push_str("nil"),
            _ => self.out.push_str("<unimplemented value>"),
        }
    }

    pub fn dump_program(&mut self, program: &MirProgram) {
        self.print_program(program);
        println!("{}", self.out);
    }
}
