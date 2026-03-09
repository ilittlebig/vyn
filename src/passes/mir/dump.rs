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
    MirValue, MirBinOp, MirPlace, MirUnOp, Capture
};

pub struct MirPrinter<'a> {
    pub program: &'a MirProgram,
    pub ctx: &'a PassContext,
    pub out: String,
    pub indent: usize,
}

impl<'a> MirPrinter<'a> {
    pub fn new(ctx: &'a PassContext, program: &'a MirProgram) -> Self {
        Self { program, ctx, out: String::new(), indent: 0 }
    }

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
        self.write_raw("}");
    }

    fn print_func(&mut self, func: &MirFunction) {
        let name = if let Some(fn_name) = self.ctx.interner.resolve(func.name) {
            fn_name
        } else {
            // should never happen
            "<unknown_symbol>"
        };

        self.begin_line();
        self.write_raw_fmt(format_args!("fn{} {}(", func.id.0, name));

        let mut index = 0;
        for param in &func.params {
            self.write_raw_fmt(format_args!("l{}", param.0));
            index += 1;
            if index != func.params.len() { self.write_raw(", "); }
        }

        self.write_raw(") {");
        self.end_line();

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
                self.print_place(dst);
                self.print_value(src);
                self.end_line();
            },
            MirStmt::Index { dst, base, index } => {
                self.begin_line();
                self.write_raw_fmt(format_args!("l{} = ", dst.0));
                self.write_raw("index ");
                self.print_value(base);
                self.write_raw(", ");
                self.print_value(index);
                self.end_line();
            },
            MirStmt::Field { dst, base, name } => {
                let name = self.ctx.interner.resolve(*name).unwrap_or("<unknown field>");
                self.begin_line();
                self.write_raw_fmt(format_args!("l{} = ", dst.0));
                self.write_raw("field ");
                self.print_value(base);
                self.write_raw(", ");
                self.write_raw_fmt(format_args!("{}", name));
                self.end_line();
            },
            MirStmt::BinOp { dst, lhs, op, rhs } => {
                self.begin_line();
                self.write_raw_fmt(format_args!("l{} = ", dst.0));
                self.print_bin_op(op);
                self.write_raw(" ");
                self.print_value(lhs);
                self.write_raw(", ");
                self.print_value(rhs);
                self.end_line();
            },
            MirStmt::UnOp { dst, op, rhs } => {
                self.begin_line();
                self.write_raw_fmt(format_args!("l{} = ", dst.0));
                self.print_un_op(op);
                self.write_raw(" ");
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

            // closures
            MirStmt::MakeClosure { dst, func, env } => {
                self.begin_line();
                self.write_raw_fmt(format_args!("l{} = mkclosure fn{}, [", dst.0, func.0));

                let mut index = 0;
                for capture in env {
                    self.print_capture(capture);
                    index += 1;
                    if index != env.len() { self.write_raw(", "); }
                }

                self.write_raw("]");
                self.end_line();
            },
            MirStmt::LoadUpvalue { dst, slot } => {
                self.line_fmt(format_args!("l{} = load_upvalue slot{}", dst.0, slot));
            },
            MirStmt::StoreUpvalue { slot, src } => {
                self.begin_line();
                self.write_raw_fmt(format_args!("store_upvalue slot{}, ", slot));
                self.print_value(src);
                self.end_line();
            },
        }
    }

    fn print_capture(&mut self, capture: &Capture) {
        match capture {
            Capture::ByRef { slot, def_id } => {
                let symbol = self.ctx.defs[def_id.0].name;
                let name = self.ctx.interner.resolve(symbol).unwrap_or("<unknown name>");
                self.write_raw_fmt(format_args!("byref {}@slot{}", name, slot));
            },
        }
    }

    fn print_place(&mut self, place: &MirPlace) {
        match place {
            MirPlace::Local(id) => self.write_raw_fmt(format_args!("l{} = ", id.0)),
            _ => self.write_raw("<unimplemented place>"),
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
                self.line_fmt(format_args!("goto bb{}", block_id.0));
            },
            Some(MirTerm::If { cond, then_bb, else_bb }) => {
                self.begin_line();
                self.write_raw("if ");
                self.print_value(cond);
                self.write_raw(" ");
                self.write_raw_fmt(format_args!("goto bb{}", then_bb.0));
                self.write_raw(" else ");
                self.write_raw_fmt(format_args!("goto bb{}", else_bb.0));
                self.end_line();
            },
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
            MirValue::ConstDouble(f) => {
                self.write_raw_fmt(format_args!("const {}", f));
            },
            MirValue::ConstBool(b) => {
                self.write_raw_fmt(format_args!("const {}", b));
            },
            MirValue::Nil => self.out.push_str("nil"),
            _ => self.out.push_str("<unimplemented value>"),
        }
    }

    fn print_bin_op(&mut self, op: &MirBinOp) {
        match op {
            // arithmetic
            MirBinOp::Add => self.write_raw("add"),
            MirBinOp::Sub => self.write_raw("sub"),
            MirBinOp::Div => self.write_raw("div"),
            MirBinOp::Mul => self.write_raw("mul"),
            MirBinOp::Mod => self.write_raw("mod"),

            // comparison
            MirBinOp::Eq => self.write_raw("eq"),
            MirBinOp::Ne => self.write_raw("neq"),
            MirBinOp::Lt => self.write_raw("lt"),
            MirBinOp::Lte => self.write_raw("lte"),
            MirBinOp::Gt => self.write_raw("gt"),
            MirBinOp::Gte => self.write_raw("gte"),

            // boolean
            MirBinOp::And => self.write_raw("and"),
            MirBinOp::Or => self.write_raw("or"),
            _ => self.write_raw("<unimplemented binary operator>"),
        }
    }

    fn print_un_op(&mut self, op: &MirUnOp) {
        match op {
            // unary
            MirUnOp::Neg => self.write_raw("neg"),
            MirUnOp::Plus => self.write_raw("plus"),
            MirUnOp::Not => self.write_raw("not"),
            _ => self.write_raw("<unimplemented unary operator>"),
        }
    }

    pub fn dump_program(&mut self) {
        self.print_program(self.program);
        println!("{}", self.out);
    }
}
