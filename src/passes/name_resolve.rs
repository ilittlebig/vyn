/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-06
 **/

use crate::passes::types::Type;
use crate::frontend::parser::{ Stmt, Expr, Block };
use crate::diagnostics::{ Span, Spanned, Diagnostic };
use crate::passes::{ PassContext, Symbol, Def, DefId, DefKind };
use crate::passes::hir::{ HirExprKind, HirExpr, HirParam, HirFunc, HirBlock, HirStmt };

fn lookup_var(ctx: &PassContext, symbol: Symbol) -> Option<DefId> {
    let mut current_scope = ctx.current_scope;
    loop {
        let scope = ctx.scope(current_scope);
        if let Some(def_id) = scope.bindings.get(&symbol) {
            return Some(*def_id);
        }
        current_scope = scope.parent?;
    }
}

fn declare_var(ctx: &mut PassContext, symbol: Symbol, span: Span, def_kind: DefKind) -> DefId {
    let existing = ctx.scope(ctx.current_scope).bindings.get(&symbol).copied();
    if let Some(def_id) = existing {
        ctx.diags.push(Diagnostic::error("redefinition of identifier", span));
        return def_id;
    }

    let def_id = DefId(ctx.defs.len());
    ctx.scope_mut(ctx.current_scope)
        .bindings
        .insert(symbol, def_id);

    ctx.defs.push(Def {
        name: symbol,
        span,
        kind: def_kind,
    });

    // make sure we have enough space to store the annotation,
    // this gets overwritten with the real type annotation later
    ctx.def_ann.push(None);
    ctx.def_types.push(Type::Any);

    def_id
}

fn use_name(ctx: &mut PassContext, name: &String, span: Span) -> HirExpr {
    let symbol = ctx.interner.intern(name);
    let Some(def_id) = lookup_var(ctx, symbol) else {
        ctx.diags.push(Diagnostic::error("use of undeclared identifier", span));
        return HirExpr { kind: HirExprKind::Error, span }
    };

    HirExpr {
        kind: HirExprKind::VarRef { def: def_id },
        span
    }
}

fn lower_function_scoped(
    ctx: &mut PassContext,
    func_name: Option<(Symbol, Span)>,
    body: &Block,
    params: &[(String, Span)]
) -> (HirBlock, Vec<HirParam>) {
    // we push scope here so function params has the same scope as the block
    ctx.push_scope();

    let mut new_params = Vec::new();
    for (name, name_span) in params {
        let symbol = ctx.interner.intern(&name);

        // warning about recusion not possible if paramter shadows function
        if matches!(func_name, Some((f_sym, f_span)) if f_sym == symbol) {
            ctx.diags.push(Diagnostic::warning(
                "parameter shadows function name",
                *name_span
            ));
        }

        declare_var(ctx, symbol, *name_span, DefKind::Param);
        let param = HirParam { name: symbol, span: *name_span };
        new_params.push(param);
    }

    let block = traverse_block(ctx, body);
    ctx.pop_scope();
    (block, new_params)
}

fn traverse_expr(ctx: &mut PassContext, expr: &Spanned<Expr>) -> HirExpr {
    let node = &expr.node;
    match node {
        Expr::String(s) => HirExpr {
            kind: HirExprKind::String(s.to_string()),
            span: expr.span,
        },
        Expr::Int(i) => HirExpr {
            kind: HirExprKind::Int(*i),
            span: expr.span,
        },
        Expr::Ident(name) => use_name(ctx, name, expr.span),
        Expr::Bool(b) => HirExpr {
            kind: HirExprKind::Bool(*b),
            span: expr.span,
        },
        Expr::Nil => HirExpr {
            kind: HirExprKind::Nil,
            span: expr.span,
        },

        Expr::Func(f) => {
            let (block, params) = lower_function_scoped(ctx, None, &f.body, &f.params);
            HirExpr {
                kind: HirExprKind::Func(HirFunc {
                    body: Box::new(block),
                    params
                }),
                span: expr.span
            }
        },

        Expr::Call { callee, args } => {
            let callee_expr = traverse_expr(ctx, callee);
            let mut new_args = Vec::new();

            for arg in args {
                let expr = traverse_expr(ctx, arg);
                new_args.push(expr);
            }

            HirExpr {
                kind: HirExprKind::Call {
                    callee: Box::new(callee_expr),
                    args: new_args
                },
                span: expr.span,
            }
        },

        Expr::Assign { target, value } => {
            let target_expr = traverse_expr(ctx, target);
            let value_expr = traverse_expr(ctx, value);

            HirExpr {
                kind: HirExprKind::Assign {
                    target: Box::new(target_expr),
                    value: Box::new(value_expr),
                },
                span: expr.span,
            }
        },

        Expr::Unary { op, rhs } => {
            let rhs_expr = traverse_expr(ctx, rhs);
            HirExpr {
                kind: HirExprKind::Unary {
                    op: op.clone(),
                    rhs: Box::new(rhs_expr),
                },
                span: expr.span,
            }
        },

        Expr::Binary { lhs, op, rhs } => {
            let lhs_expr = traverse_expr(ctx, lhs);
            let rhs_expr = traverse_expr(ctx, rhs);
            HirExpr {
                kind: HirExprKind::Binary {
                    lhs: Box::new(lhs_expr),
                    op: op.clone(),
                    rhs: Box::new(rhs_expr),
                },
                span: expr.span,
            }
        },

        Expr::Field { base, name: (name, name_span) } => {
            let base_expr = traverse_expr(ctx, base);
            let symbol = ctx.interner.intern(name);

            HirExpr {
                kind: HirExprKind::Field { base: Box::new(base_expr), name: symbol },
                span: expr.span,
            }
        },

        Expr::Index { base, index } => {
            let base_expr = traverse_expr(ctx, base);
            let index_expr = traverse_expr(ctx, index);

            HirExpr {
                kind: HirExprKind::Index { base: Box::new(base_expr), index: Box::new(index_expr) },
                span: expr.span,
            }
        },
    }
}

fn traverse_stmt(ctx: &mut PassContext, stmt: &Stmt) -> HirStmt {
    match stmt {
        Stmt::Decl { name: (name, name_span), init, ty } => {
            let symbol = ctx.interner.intern(name);
            let init = init.as_ref().map(|e| traverse_expr(ctx, e));

            // we declare var after traversing, so local x = x + 1
            // isn't valid if it's referencing itself
            let def_id = declare_var(ctx, symbol, *name_span, DefKind::LocalVar);
            ctx.def_ann[def_id.0] = ty.clone();
            HirStmt::Decl { def_id, init }
        },

        // we may need to separate these in the future, but right now they are the exact same
        Stmt::FuncDecl { name: (name, name_span), init } |
        Stmt::LocalFuncDecl { name: (name, name_span), init } => {
            let symbol = ctx.interner.intern(name);

            // here we declare var before traversing, to allow for function
            // recursion inside local function a() { a() }
            let def_id = declare_var(ctx, symbol, *name_span, DefKind::Function);

            let (block, params) = lower_function_scoped(
                ctx,
                Some((symbol, *name_span)),
                &init.body,
                &init.params
            );
            HirStmt::FuncDecl { def_id, params, init: block }
        },

        Stmt::While { cond, body, .. } => {
            let expr = traverse_expr(ctx, cond);
            let block = traverse_block_scoped(ctx, body);
            HirStmt::While { cond: expr, body: block }
        },
        Stmt::If { cond, then_block, else_block } => {
            let expr = traverse_expr(ctx, cond);
            let then_block = traverse_block_scoped(ctx, then_block);
            let else_block = else_block.as_ref().map(|b| traverse_block_scoped(ctx, b));
            HirStmt::If { cond: expr, then_block, else_block }
        },
        Stmt::Return(expr) => {
            let expr = expr.as_ref().map(|e| traverse_expr(ctx, e));
            HirStmt::Return(expr)
        },
        Stmt::Block(block) => {
            let block = traverse_block_scoped(ctx, block);
            HirStmt::Block(block)
        },
        Stmt::ExprStmt(expr) => {
            let expr = traverse_expr(ctx, expr);
            HirStmt::Expr(expr)
        },

        Stmt::Break => HirStmt::Break,
        Stmt::Continue => HirStmt::Continue,
    }
}

fn traverse_block(ctx: &mut PassContext, block: &Block) -> HirBlock {
    let mut stmts = Vec::new();
    for stmt in &block.stmts {
        let hir_stmt = traverse_stmt(ctx, &stmt);
        stmts.push(hir_stmt);
    }
    HirBlock { stmts, span: block.span }
}

fn traverse_block_scoped(ctx: &mut PassContext, block: &Block) -> HirBlock {
    ctx.push_scope();
    let block = traverse_block(ctx, block);
    ctx.pop_scope();
    block
}

pub fn run(ctx: &mut PassContext, ast: &Vec<Stmt>) -> Vec<HirStmt> {
    let mut hir = Vec::new();
    for stmt in ast {
        let hir_stmt = traverse_stmt(ctx, stmt);
        println!("{:#?}", hir_stmt);
        hir.push(hir_stmt);
    }
    hir
}
