/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-01
 **/

use std::collections::HashMap;
use std::collections::hash_map::Entry;

use crate::frontend::lexer::Operator;
use crate::frontend::parser::UnaryOp;
use crate::passes::{ PassContext, Symbol, DefId };
use crate::passes::hir::{ HirParam, HirStmt, HirStmtKind, HirExpr, HirExprKind, HirBlock };
use crate::passes::mir::{
    Builder, MirProgram, MirFunction, MirStmt, MirTerm, BasicBlock, FuncId, BlockId,
    LocalId, MirValue, MirPrinter, MirBinOp, MirUnOp, MirPlace, LoopContext, Capture
};

impl Builder<'_> {
    fn get_current_func(&mut self) -> &mut MirFunction {
        &mut self.program.funcs[self.current_func.0]
    }

    fn get_current_block(&mut self) -> &mut BasicBlock {
        let block_index = self.current_block.0;
        let func = self.get_current_func();
        &mut func.blocks[block_index]
    }

    fn set_func(&mut self, func_id: FuncId) {
        self.current_func = func_id;
    }

    fn set_block(&mut self, block_id: BlockId) {
        self.current_block = block_id;
    }

    fn push_loop(&mut self, break_bb: BlockId, continue_bb: BlockId) {
        self.loop_context.break_bb = Some(break_bb);
        self.loop_context.continue_bb = Some(continue_bb);
    }

    fn pop_loop(&mut self) {
        self.loop_context.break_bb = None;
        self.loop_context.continue_bb = None;
    }

    fn emit_stmt(&mut self, stmt: MirStmt) {
        let mut block = self.get_current_block();
        block.stmts.push(stmt);
    }

    fn terminate(&mut self, term: MirTerm) {
        let mut block = self.get_current_block();
        block.term = Some(term);
    }

    fn new_local(&mut self, def_id: &DefId) -> LocalId {
        let id = {
            let mut func = self.get_current_func();
            let id = LocalId(func.locals);
            func.locals += 1;
            id
        };

        let current_func_id = FuncId(self.current_func.0);
        self.def_to_owner[def_id.0] = Some(current_func_id);
        self.def_to_local[def_id.0] = Some(id);
        id
    }

    fn new_temp(&mut self) -> LocalId {
        let mut func = self.get_current_func();
        let id = LocalId(func.locals);
        func.locals += 1;
        id
    }

    fn new_block(&mut self) -> BlockId {
        let func = self.get_current_func();
        let id = BlockId(func.blocks.len());
        func.blocks.push(BasicBlock {
            id,
            stmts: Vec::new(),
            term: None
        });
        id
    }

    fn new_func(&mut self, name: Symbol) -> FuncId {
        let id = FuncId(self.program.funcs.len());
        self.program.funcs.push(MirFunction {
            id,
            name,
            params: Vec::new(),
            blocks: Vec::new(),
            locals: 0,

            captures: Vec::new(),
            capture_map: HashMap::new(),
        });

        self.set_func(id);
        let block_id = self.new_block();
        self.set_block(block_id);
        id
    }

    fn evaluate_cond(&mut self, cond: &HirExpr) -> MirValue {
        match &cond.kind {
            HirExprKind::Bool(b) => MirValue::ConstBool(*b),
            HirExprKind::Binary { lhs, op, rhs } => {
                let lhs = self.lower_expr(&lhs);
                let rhs = self.lower_expr(&rhs);
                let temp_id = self.new_temp();

                self.emit_stmt(MirStmt::BinOp {
                    dst: temp_id,
                    op: self.lower_binary_op(&op),
                    lhs,
                    rhs
                });
                MirValue::Local(temp_id)
            },
            HirExprKind::Unary { op, rhs } => {
                let rhs = self.lower_expr(&rhs);
                let temp_id = self.new_temp();

                self.emit_stmt(MirStmt::UnOp {
                    dst: temp_id,
                    op: self.lower_unary_op(&op),
                    rhs
                });
                MirValue::Local(temp_id)
            },
            _ => todo!() // no idea
        }
    }

    fn ensure_capture_slot(&mut self, def_id: &DefId, func_id: FuncId) -> usize {
        let func = &mut self.program.funcs[func_id.0];
        match func.capture_map.entry(*def_id) {
            Entry::Occupied(e) => *e.get(),
            Entry::Vacant(v) => {
                let s = func.captures.len();
                func.captures.push(Capture::ByRef { def_id: *def_id, slot: s });
                v.insert(s);
                s
            }
        }
    }

    fn emit_closure_value(&mut self, func_id: FuncId) -> MirValue {
        let captures = self.program.funcs[func_id.0]
            .captures
            .clone();
        let temp_id = self.new_temp();
        self.emit_stmt(MirStmt::MakeClosure { dst: temp_id, func: func_id, env: captures });
        MirValue::Local(temp_id)
    }

    fn ensure_terminated(&mut self) {
        let current_block = self.get_current_block();
        if current_block.term.is_none() {
            self.terminate(MirTerm::Return(MirValue::Nil));
        }
    }

    fn lower_func_literal(&mut self, name: Symbol, params: &[HirParam], body: &HirBlock) -> FuncId {
        let saved_func = self.current_func;
        let saved_block = self.current_block;
        let func_id = self.new_func(name);

        for param in params {
            let local_id = self.new_local(&param.def_id);
            self.get_current_func().params.push(local_id);
        }

        self.lower_into(func_id, &body.stmts);
        self.ensure_terminated();

        //let captures = self.get_current_func().captures.clone();

        self.set_func(saved_func);
        self.set_block(saved_block);
        func_id
    }

    fn lower_binary_op(&self, op: &Operator) -> MirBinOp {
        match op {
            // arithmetic
            Operator::Plus => MirBinOp::Add,
            Operator::Minus => MirBinOp::Sub,
            Operator::Division => MirBinOp::Div,
            Operator::Multiplication => MirBinOp::Mul,
            Operator::Modulus => MirBinOp::Mod,

            // comparison
            Operator::Equal => MirBinOp::Eq,
            Operator::NotEqual => MirBinOp::Ne,
            Operator::LessThan => MirBinOp::Lt,
            Operator::LessThanEqual => MirBinOp::Lte,
            Operator::GreaterThan => MirBinOp::Gt,
            Operator::GreaterThanEqual => MirBinOp::Gte,

            // boolean
            Operator::And => MirBinOp::And,
            Operator::Or => MirBinOp::Or,

            // unary
            Operator::Not => unreachable!("parser produced binary `!` (Not), this should be unary only"),
        }
    }

    fn lower_unary_op(&self, op: &UnaryOp) -> MirUnOp {
        match op {
            UnaryOp::Neg => MirUnOp::Neg,
            UnaryOp::Plus => MirUnOp::Plus,
            UnaryOp::Not => MirUnOp::Not,

        }
    }

    fn lower_place(&mut self, expr: &HirExpr) -> MirPlace {
        match &expr.kind {
            HirExprKind::VarRef { def: def_id } => {
                let Some(func_id) = self.def_to_owner[def_id.0] else {
                    unreachable!("VarRef without owner: {:?}", def_id);
                };

                let current_func_id = self.get_current_func().id;
                if current_func_id == func_id {
                    let local_id = if let Some(id) = self.def_to_local[def_id.0] {
                        id
                    } else {
                        // what do we do here, this has to error or something
                        // right now i just use a dummy value
                        LocalId(999)
                    };
                    MirPlace::Local(local_id)
                } else {
                    let slot = self.ensure_capture_slot(def_id, current_func_id);
                    MirPlace::Upvalue(slot)
                }
            }
            _ => todo!(),
        }
    }

    fn lower_expr(&mut self, expr: &HirExpr) -> MirValue {
        match &expr.kind {
            HirExprKind::Int(v) => MirValue::ConstInt(*v),
            HirExprKind::Double(v) => MirValue::ConstDouble(*v),
            HirExprKind::Bool(v) => MirValue::ConstBool(*v),
            HirExprKind::VarRef { def: def_id } => {
                let Some(func_id) = self.def_to_owner[def_id.0] else {
                    unreachable!("VarRef without owner: {:?}", def_id);
                };

                let current_func_id = self.get_current_func().id;
                if func_id != current_func_id {
                    let slot = self.ensure_capture_slot(def_id, current_func_id);
                    let temp_id = self.new_temp();
                    self.emit_stmt(MirStmt::LoadUpvalue { dst: temp_id, slot });
                    return MirValue::Local(temp_id);
                } else {
                    if let Some(id) = self.def_to_local[def_id.0] {
                        return MirValue::Local(id);
                    }
                    if let Some(id) = self.def_to_func[def_id.0] {
                        return MirValue::Func(id);
                    }
                }
                MirValue::Nil
            },
            HirExprKind::Func(func) => {
                let captures = self.program.funcs[self.current_func.0]
                    .captures
                    .clone();

                // need to do something about the anon name here
                let func_id = self.lower_func_literal(Symbol(999), &func.params, &func.body);

                let temp_id = self.new_temp();
                self.emit_stmt(MirStmt::MakeClosure { dst: temp_id, func: func_id, env: captures });
                MirValue::Local(temp_id)
            },
            HirExprKind::Binary { lhs, op, rhs } => {
                let lhs = self.lower_expr(lhs);
                let rhs = self.lower_expr(rhs);
                let temp_id = self.new_temp();

                let op = self.lower_binary_op(op);
                self.emit_stmt(MirStmt::BinOp { dst: temp_id, lhs, op, rhs });
                MirValue::Local(temp_id)
            },
            HirExprKind::Unary { op, rhs } => {
                let rhs = self.lower_expr(rhs);
                let temp_id = self.new_temp();

                let op = self.lower_unary_op(op);
                self.emit_stmt(MirStmt::UnOp { dst: temp_id, op, rhs });
                MirValue::Local(temp_id)
            },
            HirExprKind::Call { callee, args } => {
                let temp_id = self.new_temp();
                let mut new_args = Vec::new();

                for arg in args {
                    let value = self.lower_expr(arg);
                    new_args.push(value);
                }

                let callee = self.lower_expr(callee);
                self.emit_stmt(MirStmt::Call { dst: temp_id, callee, args: new_args });
                MirValue::Local(temp_id)
            },
            HirExprKind::Assign { target, value } => {
                let dst = self.lower_place(target);
                let rhs = self.lower_expr(value);

                let src = rhs.clone();
                match dst {
                    MirPlace::Local(_) => self.emit_stmt(MirStmt::Assign { dst, src }),
                    MirPlace::Upvalue(slot) => self.emit_stmt(MirStmt::StoreUpvalue { slot, src: Box::new(src) }),
                }
                rhs
            },
            HirExprKind::Index { base, index } => {
                let base = self.lower_expr(base);
                let index = self.lower_expr(index);
                let temp_id = self.new_temp();

                self.emit_stmt(MirStmt::Index { dst: temp_id, base, index });
                MirValue::Local(temp_id)
            },
            HirExprKind::Field { base, name } => {
                let base = self.lower_expr(base);
                let temp_id = self.new_temp();

                self.emit_stmt(MirStmt::Field { dst: temp_id, base, name: *name });
                MirValue::Local(temp_id)
            },
            _ => { println!("expr: {:?}", expr); todo!(); },
        }
    }

    fn lower_into(&mut self, func_id: FuncId, stmts: &[HirStmt]) {
        for stmt in stmts {
            match &stmt.kind {
                HirStmtKind::FuncDecl { def_id, init, params, .. } => {
                    let name = self.ctx.defs[def_id.0].name;
                    let new_func_id = self.lower_func_literal(name, params, init);
                    self.def_to_func[def_id.0] = Some(new_func_id);

                    let closure_value = self.emit_closure_value(new_func_id);
                    let local_id = self.new_local(def_id);
                    self.emit_stmt(MirStmt::Assign { dst: MirPlace::Local(local_id), src: closure_value });
                },
                HirStmtKind::Decl { def_id, init } => {
                    let rhs = if let Some(expr) = init {
                        self.lower_expr(expr)
                    } else {
                        MirValue::Nil
                    };

                    let local_id = self.new_local(def_id);
                    self.emit_stmt(MirStmt::Assign { dst: MirPlace::Local(local_id), src: rhs });
                },
                HirStmtKind::If { cond, then_block, else_block } => {
                    let cond_value = self.evaluate_cond(cond);

                    let then_bb = self.new_block();
                    let else_bb = else_block.as_ref().map(|_| self.new_block());
                    let join_bb = self.new_block();

                    let else_target = else_bb.unwrap_or(join_bb);
                    self.terminate(MirTerm::If { cond: cond_value, then_bb, else_bb: else_target });

                    self.set_block(then_bb);
                    self.lower_into(func_id, &then_block.stmts);
                    self.terminate(MirTerm::Goto(join_bb));

                    if let Some(else_block) = else_block {
                        self.set_block(else_target);
                        self.lower_into(func_id, &else_block.stmts);
                        self.terminate(MirTerm::Goto(join_bb));
                    }
                    self.set_block(join_bb);
                },
                HirStmtKind::While { cond, body } => {
                    let header_bb = self.new_block();
                    let body_bb = self.new_block();
                    let exit_bb = self.new_block();

                    self.push_loop(exit_bb, header_bb);
                    self.terminate(MirTerm::Goto(header_bb));

                    self.set_block(header_bb);
                    let cond_value = self.evaluate_cond(cond);
                    self.terminate(MirTerm::If { cond: cond_value, then_bb: body_bb, else_bb: exit_bb });

                    self.set_block(body_bb);
                    self.lower_into(func_id, &body.stmts);

                    let current_block = self.get_current_block();
                    if current_block.term.is_none() {
                        self.terminate(MirTerm::Goto(header_bb));
                    }

                    self.set_block(exit_bb);
                    self.pop_loop();
                },
                HirStmtKind::Block(block) => {
                    let block_bb = self.new_block();
                    let continue_bb = self.new_block();
                    self.terminate(MirTerm::Goto(block_bb));

                    self.set_block(block_bb);
                    self.lower_into(func_id, &block.stmts);

                    self.terminate(MirTerm::Goto(continue_bb));
                    self.set_block(continue_bb);
                },
                HirStmtKind::Break => {
                    // don't know what to do with these dummy values, since it should never happen
                    // but it needs to be handled
                    let break_bb = self.loop_context.break_bb.unwrap_or(BlockId(999));
                    self.terminate(MirTerm::Goto(break_bb));
                },
                HirStmtKind::Continue => {
                    // see break stmt
                    let continue_bb = self.loop_context.continue_bb.unwrap_or(BlockId(999));
                    self.terminate(MirTerm::Goto(continue_bb));
                },
                HirStmtKind::Return(expr) => {
                    let value = if let Some(expr) = expr {
                        self.lower_expr(&expr)
                    } else {
                        MirValue::Nil
                    };
                    self.terminate(MirTerm::Return(value));
                },
                HirStmtKind::Expr(expr) => { self.lower_expr(expr); },
                _ => { todo!() },
            }

            if self.get_current_block().term.is_some() {
                break;
            }
        }
    }
}

pub fn run(ctx: &mut PassContext, hir: &[HirStmt]) -> MirProgram {
    let __module_init_symbol = ctx.interner.intern("__module_init");
    let mut builder = Builder {
        ctx,

        program: MirProgram { entry: FuncId(0), funcs: Vec::new() },
        current_func: FuncId(0),
        current_block: BlockId(0),
        loop_context: LoopContext { break_bb: None, continue_bb: None },

        def_to_owner: vec![None; ctx.defs.len()],
        def_to_local: vec![None; ctx.defs.len()],
        def_to_func: vec![None; ctx.defs.len()],
    };

    let entry_id = builder.new_func(__module_init_symbol);
    builder.program.entry = entry_id;
    builder.lower_into(entry_id, hir);
    builder.terminate(MirTerm::Return(MirValue::Nil));

    builder.program
}
