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

type HirExprSpanned = Spanned<HirExpr>;

#[derive(Debug)]
enum HirExpr {
    String(String),
    Int(i64),
    Bool(bool),
    Nil,

    Func(Func),
    Call { callee: Box<HirExprSpanned>, args: Vec<HirExprSpanned> },
    Assign { target: Box<HirExprSpanned>, value: Box<HirExprSpanned> },

    Unary { op: UnaryOp, rhs: Box<HirExprSpanned> },
    Binary { lhs: Box<HirExprSpanned>, op: Operator, rhs: Box<HirExprSpanned> },

    VarRef { def: DefId },
    Error,
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
    Decl { name: Symbol, init: Option<HirExprSpanned> },
    While { cond: HirExprSpanned, body: HirBlock },
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
        return HirExpr::Error;
    };
    HirExpr::VarRef { def: def_id }
}

fn traverse_expr(ctx: &mut PassContext, expr: &Spanned<Expr>) -> HirExpr {
    let node = &expr.node;
    match node {
        Expr::String(s) => HirExpr::String(s.to_string()),
        Expr::Int(i) => HirExpr::Int(*i),
        Expr::Ident(name) => use_name(ctx, name),
        Expr::Bool(b) => HirExpr::Bool(*b),
        Expr::Nil => HirExpr::Nil,

        Expr::Func(f) => {
            let block = traverse_block(ctx, &f.body);
            HirExpr::Func(Func { body: Box::new(block) })
        },
        Expr::Call { callee, args, .. } => {
            let callee_expr = traverse_expr(ctx, callee);

            let mut new_args = Vec::new();
            for arg in args {
                let expr = traverse_expr(ctx, arg);
                new_args.push(Spanned { node: expr, span: arg.span });
            }

            HirExpr::Call {
                callee: Box::new(Spanned { node: callee_expr, span: callee.span }),
                args: new_args
            }
        },
        Expr::Assign { target, value, .. } => {
            let target_expr = traverse_expr(ctx, target);
            let value_expr = traverse_expr(ctx, value);
            HirExpr::Assign {
                target: Box::new(Spanned { node: target_expr, span: target.span }),
                value: Box::new(Spanned { node: value_expr, span: value.span }),
            }
        },
        Expr::Unary { op, rhs, .. } => {
            let rhs_expr = traverse_expr(ctx, rhs);
            HirExpr::Unary {
                op: op.clone(),
                rhs: Box::new(Spanned { node: rhs_expr, span: rhs.span }),
            }
        },
        Expr::Binary { lhs, op, rhs, .. } => {
            let lhs_expr = traverse_expr(ctx, lhs);
            let rhs_expr = traverse_expr(ctx, rhs);
            HirExpr::Binary {
                lhs: Box::new(Spanned { node: lhs_expr, span: lhs.span }),
                op: op.clone(),
                rhs: Box::new(Spanned { node: rhs_expr, span: rhs.span }),
            }
        },
    }
}

fn traverse_stmt(ctx: &mut PassContext, stmt: &Stmt) -> HirStmt {
    match stmt {
        Stmt::Decl { name: (name, name_span), init, .. } => {
            let symbol = ctx.interner.intern(name);
            let init = init.as_ref().map(|e| {
                let expr = traverse_expr(ctx, e);
                Spanned { node: expr, span: e.span }
            });

            // we declare var after traversing, so local x = x + 1
            // isn't valid if it's referencing itself
            declare_var(ctx, symbol, *name_span);
            HirStmt::Decl { name: symbol, init }
        },
        Stmt::While { cond, body, .. } => {
            let expr = traverse_expr(ctx, cond);
            let cond = Spanned { node: expr, span: cond.span };
            let block = traverse_block(ctx, body);
            HirStmt::While { cond, body: block }
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
