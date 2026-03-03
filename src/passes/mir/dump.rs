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
    MirValue, BinOp,
};

pub struct MirPrinter<'a> {
    pub program: &'a MirProgram,
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

    //
    fn line(&mut self, s: &str) {
        self.indent();
        let _ = writeln!(&mut self.out, "{s}");
    }

    fn line_fmt(&mut self, args: fmt::Arguments) {
        self.indent();
        let _ = self.out.write_fmt(args);
        self.out.push('\n');
    }

    //
    fn begin_line(&mut self) {
        self.indent();
    }

    fn end_line(&mut self) {
        self.out.push('\n');
    }

    fn write_raw(&mut self, s: &str) {
        let _ = self.out.write_str(s);
    }

    fn write_raw_fmt(&mut self, args: fmt::Arguments) {
        let _ = self.out.write_fmt(args);
    }

    //
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

        self.line_fmt(format_args!("fn{} {}() {{", func.id.0, name));
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
        match stmt {
            MirStmt::Assign { dst, src } => {
                self.begin_line();
                self.write_raw_fmt(format_args!("l{} = ", dst.0));
                self.print_value(src);
                self.end_line();
            },
            MirStmt::BinOp { dst, lhs, op, rhs } => {
                self.begin_line();
                self.write_raw_fmt(format_args!("l{} = ", dst.0));
                self.print_op(op);
                self.write_raw(" ");
                self.print_value(lhs);
                self.write_raw(", ");
                self.print_value(rhs);
                self.end_line();
            },
            MirStmt::Call { dst, callee, args } => {
                self.begin_line();
                self.write_raw_fmt(format_args!("l{} = call ", dst.0));
                self.print_value(callee);
                self.write_raw(", [");

                let mut index = 0;
                for arg in args {
                    self.print_value(arg);
                    index += 1;
                    if index != args.len() { self.write_raw(", "); }
                }
                self.write_raw("]");
                self.end_line();
            },
        }
    }

    fn print_term(&mut self, term: &Option<MirTerm>) {
        match term {
            Some(MirTerm::Return(value)) => {
                self.begin_line();
                self.write_raw("return ");
                self.print_value(value);
                self.end_line();
            },
            Some(MirTerm::Goto(block_id)) => {
                self.line_fmt(format_args!("goto {}", block_id.0));
            },
            Some(MirTerm::If { .. }) => self.line("<unimplemented terminator>"),
            None => self.line("<missing terminator>"),
        }
    }

    fn print_value(&mut self, value: &MirValue) {
        match value {
            MirValue::Func(id) => {
                let func = &self.program.funcs[id.0];
                let name = self.ctx.interner.resolve(func.name).unwrap_or("<unknown function>");
                self.write_raw_fmt(format_args!("{}", name));
            },
            MirValue::Local(id) => {
                self.write_raw_fmt(format_args!("l{}", id.0));
            },
            MirValue::ConstInt(i) => {
                self.write_raw_fmt(format_args!("const {}", i));
            },
            MirValue::Nil => self.out.push_str("nil"),
            _ => self.out.push_str("<unimplemented value>"),
        }
    }

    fn print_op(&mut self, op: &BinOp) {
        match op {
            BinOp::Add => self.write_raw("add"),
            BinOp::Minus => self.write_raw("sub"),
            BinOp::Division => self.write_raw("div"),
            BinOp::Multiplication => self.write_raw("mul"),
            BinOp::Modulus => self.write_raw("mod"),
            _ => self.write_raw("<unimplemented operator>"),
        }
    }

    pub fn dump_program(&mut self) {
        self.print_program(self.program);
        println!("{}", self.out);
    }
}
