/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-06
 **/

use crate::diagnostics::{ Span, Spanned };
use crate::passes::{ PassContext, Symbol, Def, DefId, DefKind, Scope };
use crate::frontend::parser::{ Stmt, Expr, Block, UnaryOp };
use crate::frontend::lexer::Operator;

#[derive(Debug)]
enum HirExprKind {
    String(String),
    Int(i64),
    Bool(bool),
    Nil,

    Func(Func),
    Call { callee: Box<HirExpr>, args: Vec<HirExpr> },
    Assign { target: Box<HirExpr>, value: Box<HirExpr> },

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
struct Func {
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
    While { cond: HirExpr, body: HirBlock },
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

// TODO: do we emit redefinition diagnostics here too?
fn declare_var(ctx: &mut PassContext, symbol: Symbol, span: Span) {
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

fn use_name(ctx: &mut PassContext, name: &String) -> HirExpr {
    let symbol = ctx.interner.intern(name);
    let Some(def_id) = lookup_var(ctx, symbol) else {
        return HirExpr {
            kind: HirExprKind::Error,
            span: Span { start: 0, end: 0 },
        }
    };

    HirExpr {
        kind: HirExprKind::VarRef { def: def_id },
        span: Span { start: 0, end: 0 },
    }
}

fn traverse_expr(ctx: &mut PassContext, expr: &Spanned<Expr>) -> HirExpr {
    let node = &expr.node;
    match node {
        Expr::String(s) => HirExpr {
            kind: HirExprKind::String(s.to_string()),
            span: Span { start: 0, end: 0 },
        },
        Expr::Int(i) => HirExpr {
            kind: HirExprKind::Int(*i),
            span: Span { start: 0, end: 0 },
        },
        Expr::Ident(name) => use_name(ctx, name),
        Expr::Bool(b) => HirExpr {
            kind: HirExprKind::Bool(*b),
            span: Span { start: 0, end: 0 },
        },
        Expr::Nil => HirExpr {
            kind: HirExprKind::Nil,
            span: Span { start: 0, end: 0 },
        },

        Expr::Func(f) => {
            let block = traverse_block(ctx, &f.body);
            let span = block.span.clone();

            HirExpr {
                kind: HirExprKind::Func(Func { body: Box::new(block) }),
                span
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
                span: Span { start: 0, end: 0 },
            }
        },

        Expr::Assign { target, value, .. } => {
            let target_expr = traverse_expr(ctx, target);
            let value_expr = traverse_expr(ctx, value);

            HirExpr {
                kind: HirExprKind::Assign {
                    target: Box::new(target_expr),
                    value: Box::new(value_expr),
                },
                span: Span { start: 0, end: 0 },
            }
        },

        Expr::Unary { op, rhs, .. } => {
            let rhs_expr = traverse_expr(ctx, rhs);
            HirExpr {
                kind: HirExprKind::Unary {
                    op: op.clone(),
                    rhs: Box::new(rhs_expr),
                },
                span: Span { start: 0, end: 0 },
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
                span: Span { start: 0, end: 0 },
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
        Stmt::While { cond, body, .. } => {
            let expr = traverse_expr(ctx, cond);
            let block = traverse_block(ctx, body);
            HirStmt::While { cond: expr, body: block }
        },
        _ => { todo!(); },
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
