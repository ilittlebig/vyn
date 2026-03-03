/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-01
 **/

use crate::frontend::lexer::Operator;
use crate::passes::{ PassContext, Symbol, DefId };
use crate::passes::hir::{ HirStmt, HirStmtKind, HirExpr, HirExprKind };
use crate::passes::mir::{
    Builder, MirProgram, MirFunction, MirStmt, MirTerm, BasicBlock, FuncId, BlockId,
    LocalId, MirValue, MirPrinter, BinOp, MirPlace, LoopContext
};

impl Builder {
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
                    op: self.lower_op(&op),
                    lhs,
                    rhs
                });
                MirValue::Local(temp_id)
            },
            _ => todo!() // no idea
        }
    }

    fn lower_op(&self, op: &Operator) -> BinOp {
        match op {
            // arithmetic
            Operator::Plus => BinOp::Add,
            Operator::Minus => BinOp::Minus,
            Operator::Division => BinOp::Division,
            Operator::Multiplication => BinOp::Multiplication,
            Operator::Modulus => BinOp::Modulus,

            // comparison
            Operator::Equal => BinOp::Equal,
            Operator::NotEqual => BinOp::NotEqual,
            Operator::LessThan => BinOp::LessThan,
            Operator::LessThanEqual => BinOp::LessThanEqual,
            Operator::GreaterThan => BinOp::GreaterThan,
            Operator::GreaterThanEqual => BinOp::GreaterThanEqual,
            _ => { todo!(); }
        }
    }

    fn lower_place(&mut self, expr: &HirExpr) -> MirPlace {
        match &expr.kind {
            HirExprKind::VarRef { def: def_id } => {
                let local_id = if let Some(id) = self.def_to_local[def_id.0] {
                    id
                } else {
                    // what do we do here, this has to error or something
                    // right now i just use a dummy value
                    LocalId(999)
                };
                MirPlace::Local(local_id)
            }
            _ => todo!(),
        }
    }

    fn lower_expr(&mut self, expr: &HirExpr) -> MirValue {
        match &expr.kind {
            HirExprKind::Int(v) => MirValue::ConstInt(*v),
            HirExprKind::Double(v) => MirValue::ConstDouble(*v),
            HirExprKind::VarRef { def: def_id } => {
                if let Some(id) = self.def_to_local[def_id.0] {
                    return MirValue::Local(id);
                }
                if let Some(id) = self.def_to_func[def_id.0] {
                    return MirValue::Func(id);
                }
                MirValue::Nil
            },
            HirExprKind::Binary { lhs, op, rhs } => {
                let lhs = self.lower_expr(lhs);
                let rhs = self.lower_expr(rhs);

                let temp_id = self.new_temp();
                let op = self.lower_op(op);
                self.emit_stmt(MirStmt::BinOp { dst: temp_id, lhs, op, rhs });
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
                let target_place = self.lower_place(target);
                let rhs = self.lower_expr(value);
                self.emit_stmt(MirStmt::Assign { dst: target_place, src: rhs.clone() });
                rhs
            },
            _ => MirValue::ConstBool(false),
        }
    }

    fn lower_into(&mut self, ctx: &PassContext, func_id: FuncId, stmts: &[HirStmt]) {
        for stmt in stmts {
            match &stmt.kind {
                HirStmtKind::FuncDecl { def_id, init, params, .. } => {
                    let start_block_id = self.current_block;

                    let name = ctx.defs[def_id.0].name;
                    let new_func_id = self.new_func(name);
                    self.def_to_func[def_id.0] = Some(new_func_id);

                    for param in params {
                        let local_id = self.new_local(&param.def_id);
                        self.get_current_func().params.push(local_id);
                    }

                    self.lower_into(ctx, new_func_id, &init.stmts);
                    let current_block = self.get_current_block();
                    if current_block.term.is_none() {
                        self.terminate(MirTerm::Return(MirValue::Nil));
                    }

                    self.set_func(func_id);
                    self.set_block(start_block_id);
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
                    self.lower_into(ctx, func_id, &then_block.stmts);
                    self.terminate(MirTerm::Goto(join_bb));

                    if let Some(else_block) = else_block {
                        self.set_block(else_target);
                        self.lower_into(ctx, func_id, &else_block.stmts);
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
                    self.lower_into(ctx, func_id, &body.stmts);

                    let current_block = self.get_current_block();
                    if current_block.term.is_none() {
                        self.terminate(MirTerm::Goto(header_bb));
                    }

                    self.set_block(exit_bb);
                    self.pop_loop();
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
        program: MirProgram { entry: FuncId(0), funcs: Vec::new() },
        current_func: FuncId(0),
        current_block: BlockId(0),
        loop_context: LoopContext { break_bb: None, continue_bb: None },

        def_to_local: vec![None; ctx.defs.len()],
        def_to_func: vec![None; ctx.defs.len()],
    };

    let entry_id = builder.new_func(__module_init_symbol);
    builder.program.entry = entry_id;
    builder.lower_into(ctx, entry_id, hir);
    builder.terminate(MirTerm::Return(MirValue::Nil));

    // maybe we should have some arg to show this too?
    // println!("{:#?}", builder);

    // this should only be here if the user passes --dump-hir
    let mut mir_printer = MirPrinter {
        program: &builder.program,
        ctx,
        out: String::new(),
        indent: 0,
    };
    mir_printer.dump_program();

    builder.program
}
