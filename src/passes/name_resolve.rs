/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-06
 **/

use crate::diagnostics::{ Span, Spanned, Diagnostic };
use crate::passes::{ PassContext, Symbol, Def, DefId, DefKind };
use crate::frontend::parser::{ Stmt, Expr, Block, UnaryOp };
use crate::frontend::lexer::Operator;

#[derive(Debug)]
enum HirExprKind {
    String(String),
    Int(i64),
    Bool(bool),
    Nil,

    Func(HirFunc),
    Call { callee: Box<HirExpr>, args: Vec<HirExpr> },
    Assign { target: Box<HirExpr>, value: Box<HirExpr> },

    Field { base: Box<HirExpr>, name: Symbol },
    Index { base: Box<HirExpr>, index: Box<HirExpr> },

    Unary { op: UnaryOp, rhs: Box<HirExpr> },
    Binary { lhs: Box<HirExpr>, op: Operator, rhs: Box<HirExpr> },

    VarRef { def: DefId },
    Error,
}

#[derive(Debug)]
struct HirExpr {
    kind: HirExprKind,
    span: Span,
}

#[derive(Debug)]
struct HirFunc {
    body: Box<HirBlock>,
    //TODO: params
}

#[derive(Debug)]
struct HirBlock {
    stmts: Vec<HirStmt>,
    span: Span,
}

#[derive(Debug)]
enum HirStmt {
    Decl { name: Symbol, init: Option<HirExpr> },
    FuncDecl { name: Symbol, init: HirBlock },
    While { cond: HirExpr, body: HirBlock },
    If { cond: HirExpr, then_block: HirBlock, else_block: Option<HirBlock> },

    Return(Option<HirExpr>),
    Break,
    Continue,

    Block(HirBlock),
    Expr(HirExpr),
}

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

fn declare_var(ctx: &mut PassContext, symbol: Symbol, span: Span) {
    if ctx.scope(ctx.current_scope).bindings.get(&symbol).is_some() {
        ctx.diags.push(Diagnostic::error("redefinition of identifier", span));
        return;
    }

    let def_id = DefId(ctx.defs.len());
    ctx.scope_mut(ctx.current_scope)
        .bindings
        .insert(symbol, def_id);

    ctx.defs.push(Def {
        name: symbol,
        span,
        kind: DefKind::LocalVar
    });
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
            let block = traverse_block(ctx, &f.body);
            HirExpr {
                kind: HirExprKind::Func(HirFunc { body: Box::new(block) }),
                span: expr.span
            }
        },

        Expr::Call { callee, args, .. } => {
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

        Expr::Assign { target, value, .. } => {
            let target_expr = traverse_expr(ctx, target);
            let value_expr = traverse_expr(ctx, value);

            println!("{:?}", target_expr);

            HirExpr {
                kind: HirExprKind::Assign {
                    target: Box::new(target_expr),
                    value: Box::new(value_expr),
                },
                span: expr.span,
            }
        },

        Expr::Unary { op, rhs, .. } => {
            let rhs_expr = traverse_expr(ctx, rhs);
            HirExpr {
                kind: HirExprKind::Unary {
                    op: op.clone(),
                    rhs: Box::new(rhs_expr),
                },
                span: expr.span,
            }
        },

        Expr::Binary { lhs, op, rhs, .. } => {
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
        Stmt::Decl { name: (name, name_span), init, .. } => {
            let symbol = ctx.interner.intern(name);
            let init = init.as_ref().map(|e| traverse_expr(ctx, e));

            // we declare var after traversing, so local x = x + 1
            // isn't valid if it's referencing itself
            declare_var(ctx, symbol, *name_span);
            HirStmt::Decl { name: symbol, init }
        },
        Stmt::FuncDecl { name: (name, name_span), init } => {
            let symbol = ctx.interner.intern(name);
            declare_var(ctx, symbol, *name_span);
            let body = traverse_block(ctx, &init.body);
            HirStmt::FuncDecl { name: symbol, init: body }
        },
        Stmt::While { cond, body, .. } => {
            let expr = traverse_expr(ctx, cond);
            let block = traverse_block(ctx, body);
            HirStmt::While { cond: expr, body: block }
        },
        Stmt::If { cond, then_block, else_block } => {
            let expr = traverse_expr(ctx, cond);
            let then_block = traverse_block(ctx, then_block);
            let else_block = else_block.as_ref().map(|b| traverse_block(ctx, b));
            HirStmt::If { cond: expr, then_block, else_block }
        },
        Stmt::Return(expr) => {
            let expr = expr.as_ref().map(|e| traverse_expr(ctx, e));
            HirStmt::Return(expr)
        },
        Stmt::Break => HirStmt::Break,
        Stmt::Continue => HirStmt::Continue,
        Stmt::Block(block) => {
            let block = traverse_block(ctx, block);
            HirStmt::Block(block)
        },
        Stmt::ExprStmt(expr) => {
            let expr = traverse_expr(ctx, expr);
            HirStmt::Expr(expr)
        },
    }
}

fn traverse_block(ctx: &mut PassContext, block: &Block) -> HirBlock {
    let mut stmts = Vec::new();
    ctx.push_scope();

    for stmt in &block.stmts {
        let hir_stmt = traverse_stmt(ctx, &stmt);
        stmts.push(hir_stmt);
    }

    ctx.pop_scope();
    HirBlock { stmts, span: block.span }
}

pub fn run(ctx: &mut PassContext, ast: &Vec<Stmt>) {
    for stmt in ast {
        let hir_stmt = traverse_stmt(ctx, stmt);
        println!("{:#?}", hir_stmt);
    }
}
