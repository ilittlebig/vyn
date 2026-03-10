/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-03
 **/

use std::fmt::{ self, Write };

use crate::tools::fmt::Printer;
use crate::passes::PassContext;
use crate::passes::mir::{
    MirProgram, MirFunction, MirStmt, MirTerm, BasicBlock,
    MirValue, MirBinOp, MirPlace, MirUnOp, Capture
};

pub struct MirDumper<'a, W: Write> {
    pub p: &'a mut Printer<W>,
    pub program: &'a MirProgram,
    pub ctx: &'a PassContext,
}

impl<'a, W: Write> MirDumper<'a, W> {
    pub fn new(printer: &'a mut Printer<W>, ctx: &'a PassContext, program: &'a MirProgram) -> Self {
        Self { p: printer, program, ctx }
    }

    pub fn dump_program(&mut self) {
        self.print_program(self.program);
    }

    fn print_program(&mut self, program: &MirProgram) {
        self.p.line("program {");
        let ctx = self.ctx;
        self.p.with_indent(|p| {
            p.line_fmt(format_args!("entry: fn{}", program.entry.0));
            p.line("");

            for func in &program.funcs {
                Self::print_func_inner(p, ctx, program, func);
            }
        });
        self.p.write_raw("}");
    }

    fn print_func_inner(p: &mut Printer<W>, ctx: &PassContext, program: &MirProgram, func: &MirFunction) {
        let name = if let Some(fn_name) = ctx.interner.resolve(func.name) {
            fn_name
        } else {
            // should never happen
            "<unknown_symbol>"
        };

        p.begin_line();
        p.write_raw_fmt(format_args!("fn{} {}(", func.id.0, name));

        let mut index = 0;
        for param in &func.params {
            p.write_raw_fmt(format_args!("l{}", param.0));
            index += 1;
            if index != func.params.len() { p.write_raw(", "); }
        }

        p.write_raw(") {");
        p.end_line();

        p.with_indent(|p| {
            for block in &func.blocks {
                Self::print_block_inner(p, ctx, program, block);
            }
        });

        p.line("}");
        p.line("");
    }

    fn print_block_inner(p: &mut Printer<W>, ctx: &PassContext, program: &MirProgram, block: &BasicBlock) {
        p.line_fmt(format_args!("bb{}:", block.id.0));
        p.with_indent(|p| {
            for stmt in &block.stmts {
                Self::print_stmt_inner(p, ctx, program, stmt);
            }
            Self::print_term_inner(p, ctx, program, &block.term);
        });
    }

    fn print_stmt_inner(p: &mut Printer<W>, ctx: &PassContext, program: &MirProgram, stmt: &MirStmt) {
        match stmt {
            MirStmt::Assign { dst, src } => {
                p.begin_line();
                Self::print_place_inner(p, dst);
                Self::print_value_inner(p, ctx, program, src);
                p.end_line();
            },
            MirStmt::Index { dst, base, index } => {
                p.begin_line();
                p.write_raw_fmt(format_args!("l{} = ", dst.0));
                p.write_raw("index ");
                Self::print_value_inner(p, ctx, program, base);
                p.write_raw(", ");
                Self::print_value_inner(p, ctx, program, index);
                p.end_line();
            },
            MirStmt::Field { dst, base, name } => {
                let name = ctx.interner.resolve(*name).unwrap_or("<unknown field>");
                p.begin_line();
                p.write_raw_fmt(format_args!("l{} = ", dst.0));
                p.write_raw("field ");
                Self::print_value_inner(p, ctx, program, base);
                p.write_raw(", ");
                p.write_raw_fmt(format_args!("{}", name));
                p.end_line();
            },
            MirStmt::BinOp { dst, lhs, op, rhs } => {
                p.begin_line();
                p.write_raw_fmt(format_args!("l{} = ", dst.0));
                Self::print_bin_op_inner(p, op);
                p.write_raw(" ");
                Self::print_value_inner(p, ctx, program, lhs);
                p.write_raw(", ");
                Self::print_value_inner(p, ctx, program, rhs);
                p.end_line();
            },
            MirStmt::UnOp { dst, op, rhs } => {
                p.begin_line();
                p.write_raw_fmt(format_args!("l{} = ", dst.0));
                Self::print_un_op_inner(p, op);
                p.write_raw(" ");
                Self::print_value_inner(p, ctx, program, rhs);
                p.end_line();
            },
            MirStmt::Call { dst, callee, args } => {
                p.begin_line();
                p.write_raw_fmt(format_args!("l{} = call ", dst.0));
                Self::print_value_inner(p, ctx, program, callee);
                p.write_raw(", [");

                let mut index = 0;
                for arg in args {
                    Self::print_value_inner(p, ctx, program, arg);
                    index += 1;
                    if index != args.len() { p.write_raw(", "); }
                }

                p.write_raw("]");
                p.end_line();
            },

            // closures
            MirStmt::MakeClosure { dst, func, env } => {
                p.begin_line();
                p.write_raw_fmt(format_args!("l{} = mkclosure fn{}, [", dst.0, func.0));

                let mut index = 0;
                for capture in env {
                    Self::print_capture_inner(p, ctx, capture);
                    index += 1;
                    if index != env.len() { p.write_raw(", "); }
                }

                p.write_raw("]");
                p.end_line();
            },
            MirStmt::LoadUpvalue { dst, slot } => {
                p.line_fmt(format_args!("l{} = load_upvalue slot{}", dst.0, slot));
            },
            MirStmt::StoreUpvalue { slot, src } => {
                p.begin_line();
                p.write_raw_fmt(format_args!("store_upvalue slot{}, ", slot));
                Self::print_value_inner(p, ctx, program, src);
                p.end_line();
            },
        }
    }

    fn print_capture_inner(p: &mut Printer<W>, ctx: &PassContext, capture: &Capture) {
        match capture {
            Capture::ByRef { slot, def_id } => {
                let symbol = ctx.defs[def_id.0].name;
                let name = ctx.interner.resolve(symbol).unwrap_or("<unknown name>");
                p.write_raw_fmt(format_args!("byref {}@slot{}", name, slot));
            },
        }
    }

    fn print_place_inner(p: &mut Printer<W>, place: &MirPlace) {
        match place {
            MirPlace::Local(id) => p.write_raw_fmt(format_args!("l{} = ", id.0)),
            _ => p.write_raw("<unimplemented place>"),
        }
    }

    fn print_term_inner(p: &mut Printer<W>, ctx: &PassContext, program: &MirProgram, term: &Option<MirTerm>) {
        match term {
            Some(MirTerm::Return(value)) => {
                p.begin_line();
                p.write_raw("return ");
                Self::print_value_inner(p, ctx, program, value);
                p.end_line();
            },
            Some(MirTerm::Goto(block_id)) => {
                p.line_fmt(format_args!("goto bb{}", block_id.0));
            },
            Some(MirTerm::If { cond, then_bb, else_bb }) => {
                p.begin_line();
                p.write_raw("if ");
                Self::print_value_inner(p, ctx, program, cond);
                p.write_raw(" ");
                p.write_raw_fmt(format_args!("goto bb{}", then_bb.0));
                p.write_raw(" else ");
                p.write_raw_fmt(format_args!("goto bb{}", else_bb.0));
                p.end_line();
            },
            None => p.line("<missing terminator>"),
        }
    }

    fn print_value_inner(p: &mut Printer<W>, ctx: &PassContext, program: &MirProgram, value: &MirValue) {
        match value {
            MirValue::Func(id) => {
                let func = &program.funcs[id.0];
                let name = ctx.interner.resolve(func.name).unwrap_or("<unknown function>");
                p.write_raw_fmt(format_args!("{}", name));
            },
            MirValue::Local(id) => {
                p.write_raw_fmt(format_args!("l{}", id.0));
            },
            MirValue::ConstInt(i) => {
                p.write_raw_fmt(format_args!("const {}", i));
            },
            MirValue::ConstDouble(f) => {
                p.write_raw_fmt(format_args!("const {}", f));
            },
            MirValue::ConstBool(b) => {
                p.write_raw_fmt(format_args!("const {}", b));
            },
            MirValue::ConstString(v) => {
                p.write_raw_fmt(format_args!("const {}", v));
            },
            MirValue::Nil => p.write_raw("nil"),
            _ => p.write_raw("<unimplemented value>"),
        }
    }

    fn print_bin_op_inner(p: &mut Printer<W>, op: &MirBinOp) {
        match op {
            // arithmetic
            MirBinOp::Add => p.write_raw("add"),
            MirBinOp::Sub => p.write_raw("sub"),
            MirBinOp::Div => p.write_raw("div"),
            MirBinOp::Mul => p.write_raw("mul"),
            MirBinOp::Mod => p.write_raw("mod"),

            // comparison
            MirBinOp::Eq => p.write_raw("eq"),
            MirBinOp::Ne => p.write_raw("neq"),
            MirBinOp::Lt => p.write_raw("lt"),
            MirBinOp::Lte => p.write_raw("lte"),
            MirBinOp::Gt => p.write_raw("gt"),
            MirBinOp::Gte => p.write_raw("gte"),

            // boolean
            MirBinOp::And => p.write_raw("and"),
            MirBinOp::Or => p.write_raw("or"),
            _ => p.write_raw("<unimplemented binary operator>"),
        }
    }

    fn print_un_op_inner(p: &mut Printer<W>, op: &MirUnOp) {
        match op {
            // unary
            MirUnOp::Neg => p.write_raw("neg"),
            MirUnOp::Plus => p.write_raw("plus"),
            MirUnOp::Not => p.write_raw("not"),
            _ => p.write_raw("<unimplemented unary operator>"),
        }
    }
}
