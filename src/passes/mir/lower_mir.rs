/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-01
 **/

use crate::passes::{ PassContext, Symbol };
use crate::passes::hir::{ HirStmt, HirStmtKind, HirExpr, HirExprKind };
use crate::passes::mir::{
    Builder, MirProgram, MirFunction, MirStmt, MirTerm, BasicBlock, FuncId, BlockId,
    LocalId, MirValue, MirPrinter,
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

    fn emit_stmt(&mut self, stmt: MirStmt) {
        let mut block = self.get_current_block();
        block.stmts.push(stmt);
    }

    fn terminate(&mut self, term: MirTerm) {
        let mut block = self.get_current_block();
        block.term = Some(term);
    }

    fn new_local(&mut self) -> LocalId {
        let mut func = self.get_current_func();
        let id = LocalId(func.locals);
        func.locals += 1;
        id
    }

    fn new_block(&mut self, func_id: FuncId) -> BlockId {
        let func = self.get_current_func();
        let id = BlockId(func.blocks.len());
        func.blocks.push(BasicBlock {
            id,
            stmts: Vec::new(),
            term: None
        });

        self.set_block(id);
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
        self.new_block(id);
        id
    }

    fn lower_expr(&self, expr: &HirExpr) -> MirValue {
        match &expr.kind {
            HirExprKind::Int(v) => MirValue::ConstInt(*v),
            _ => MirValue::ConstBool(false),
        }
    }

    fn lower_into(&mut self, ctx: &PassContext, func_id: FuncId, stmts: &[HirStmt]) {
        for stmt in stmts {
            match &stmt.kind {
                HirStmtKind::FuncDecl { def_id, init, .. } => {
                    let name = ctx.defs[def_id.0].name;
                    let new_func_id = self.new_func(name);
                    self.lower_into(ctx, new_func_id, &init.stmts);
                    self.set_func(func_id);
                },
                HirStmtKind::Return(expr) => {
                    let value = if let Some(expr) = expr {
                        self.lower_expr(&expr)
                    } else {
                        MirValue::Nil
                    };
                    self.terminate(MirTerm::Return(value));
                },
                _ => {},
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
    };

    let entry_id = builder.new_func(__module_init_symbol);
    builder.program.entry = entry_id;
    builder.lower_into(ctx, entry_id, hir);
    builder.terminate(MirTerm::Return(MirValue::Nil));

    // maybe we should have some arg to show this too?
    // println!("{:#?}", builder);

    // this should only be here if the user passes --dump-hir
    let mut mir_printer = MirPrinter {
        ctx,
        out: String::new(),
        indent: 0,
    };
    mir_printer.dump_program(&builder.program);

    builder.program
}
