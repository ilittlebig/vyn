/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-24
 **/

use crate::passes::types;
use crate::passes::types::Type;

use crate::passes::PassContext;
use crate::diagnostics::Diagnostic;
use crate::passes::hir::{ HirStmt, HirStmtKind, HirExpr, HirBlock, HirExprKind };

struct ContextStack {
    function_depth: usize,
    loop_depth: usize,
    expected_return: Option<Type>,
}

fn expect_cond(ctx: &mut PassContext, cond: &HirExpr) {
    let cond_ty = types::type_expr(ctx, cond);
    let is_bool = cond_ty == Type::Named(ctx.builtin_types.bool);
    let is_ok = is_bool || cond_ty == Type::Error || cond_ty == Type::Any;

    if !is_ok {
        let msg = format!(
            "expected '{}', got '{}'",
            types::fmt_type(ctx, &Type::Named(ctx.builtin_types.bool)),
            types::fmt_type(ctx, &cond_ty)
        );
        ctx.diags.push(Diagnostic::error(msg, cond.span));
    }
}

fn check_block(ctx: &mut PassContext, ctx_stack: &mut ContextStack, block: &HirBlock) {
    for block_stmt in &block.stmts {
        check_stmt(ctx, ctx_stack, &block_stmt);
    }
}

fn stmt_def_returns(stmt: &HirStmt) -> bool {
    match &stmt.kind {
        HirStmtKind::Return(_) => true,
        HirStmtKind::Block(b) => block_def_returns(b),
        HirStmtKind::If { then_block, else_block, .. } =>
            block_def_returns(then_block) &&
            else_block.as_ref().map(block_def_returns).unwrap_or(false),
        _ => false,
    }
}

fn block_def_returns(block: &HirBlock) -> bool {
    for stmt in &block.stmts {
        if stmt_def_returns(&stmt) { return true; }
    }
    false
}

fn check_stmt(ctx: &mut PassContext, ctx_stack: &mut ContextStack, stmt: &HirStmt) {
    match &stmt.kind {
        HirStmtKind::Decl { def_id, init } => {
            let name_span = ctx.defs[def_id.0].span;
            let ann = ctx.def_ann[def_id.0].clone();

            let annotated = ann.as_ref().map(|t| {
                let (ty, ty_span) = types::lower_type_ref(ctx, &t);
                if matches!(ty, Type::Error) {
                    ctx.diags.push(Diagnostic::error("unknown type name", ty_span));
                }
                ty
            });

            let inferred = init.as_ref().map(|e| types::type_expr(ctx, e));
            let def_type = annotated.clone().or(inferred.clone()).unwrap_or(Type::Any);
            ctx.def_types[def_id.0] = def_type;

            if let (Some(ann), Some(inf)) = (annotated, inferred) {
                // we will get cascading errors here unless we check if either side
                // was of type error
                if !types::assignable(&ann, &inf) && ann != Type::Error && inf != Type::Error {
                    let span = init.as_ref().map(|e| e.span).unwrap_or(name_span);
                    let msg = format!(
                        "cannot assign '{}' to '{}'",
                        types::fmt_type(ctx, &inf),
                        types::fmt_type(ctx, &ann)
                    );
                    ctx.diags.push(Diagnostic::error(msg, span));
                }
            }

            // local x = function(...) { ... }
            if let Some(HirExprKind::Func(func)) = init.as_ref().map(|e| &e.kind) {
                ctx_stack.function_depth += 1;
                check_block(ctx, ctx_stack, &func.body);
                ctx_stack.function_depth -= 1;
            }
        },

        HirStmtKind::FuncDecl { def_id, params, init, ret } => {
            if let Some((ret_type, ret_span)) = &ret {
                if matches!(ret_type, Type::Error) {
                    ctx.diags.push(Diagnostic::error("unknown type name", *ret_span));
                }

                ctx_stack.expected_return = Some(ret_type.clone());

                if !block_def_returns(&init) {
                    let msg = format!(
                        "not all paths return a value of type '{}'",
                        types::fmt_type(ctx, &ret_type)
                    );
                    ctx.diags.push(Diagnostic::error(msg, *ret_span));
                }
            }


            let mut new_params = Vec::new();
            for param in params {
                let def_id = param.def_id;
                let ty = ctx.def_types[def_id.0].clone();

                if matches!(ty, Type::Error) {
                    ctx.diags.push(Diagnostic::error("unknown type for parameter", param.span));
                }
                new_params.push(ty);
            }

            ctx.def_types[def_id.0] = Type::Func {
                params: new_params,
                ret: Box::new(Type::Any)
            };

            ctx_stack.function_depth += 1;
            check_block(ctx, ctx_stack, &init);
            ctx_stack.function_depth -= 1;
        },

        HirStmtKind::If { cond, then_block, else_block } => {
            expect_cond(ctx, &cond);
            check_block(ctx, ctx_stack, &then_block);
            if let Some(else_block) = else_block {
                check_block(ctx, ctx_stack, &else_block);
            }
        }

        HirStmtKind::While { cond, body } => {
            expect_cond(ctx, &cond);
            ctx_stack.loop_depth += 1;
            check_block(ctx, ctx_stack, &body);
            ctx_stack.loop_depth -= 1;
        },

        HirStmtKind::Return(expr) => {
            if ctx_stack.function_depth == 0 {
                let msg = "cannot return outside function declaration";
                ctx.diags.push(Diagnostic::error(msg, stmt.span));
            }

            if let Some(expected) = &ctx_stack.expected_return {
                if let Some(expr) = &expr {
                    let actual = types::type_expr(ctx, expr);
                    let mismatch = *expected != actual && *expected != Type::Any;

                    if mismatch {
                        let msg = format!(
                            "return does not match expected type '{}'",
                            types::fmt_type(ctx, expected)
                        );
                        ctx.diags.push(Diagnostic::error(msg, stmt.span));
                    }
                }
            }
        },

        HirStmtKind::Expr(expr) => { types::type_expr(ctx, &expr); },
        HirStmtKind::Block(block) => check_block(ctx, ctx_stack, &block),
        _ => {},
    }
}

pub fn run(ctx: &mut PassContext, hir: &[HirStmt]) {
    let mut ctx_stack = ContextStack {
        function_depth: 0,
        loop_depth: 0,
        expected_return: None,
    };

    for stmt in hir {
        check_stmt(ctx, &mut ctx_stack, stmt);
    }
}
