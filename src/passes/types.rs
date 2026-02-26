/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-25
 **/

use crate::frontend::parser::TypeRef;
use crate::frontend::lexer::Operator;

use crate::diagnostics::{ Span, Diagnostic };
use crate::passes::{ PassContext, Symbol, DefId };
use crate::passes::hir::{ HirExprKind, HirExpr, HirStmt, HirBlock, HirParam };

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TypeId(pub usize);

pub struct BuiltinTypes {
    pub string: TypeId,
    pub int: TypeId,
    pub bool: TypeId,
    pub nil: TypeId,
    pub any: TypeId,
}

impl BuiltinTypes {
    pub fn dummy() -> Self {
        let z = TypeId(0);
        BuiltinTypes { string: z, int: z, bool: z, nil: z, any: z }
    }
}

pub enum TypeDefKind {
    Builtin,
    // Struct { ... },
    // Enum { ... },
}

pub struct TypeDef {
    pub name: Symbol,
    pub kind: TypeDefKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Named(TypeId),
    Func { params: Vec<Type>, ret: Box<Type> },
    Any,
    Error,
}

pub struct FuncInfo<'a> {
    pub def_id: &'a DefId,
    pub params: &'a Vec<HirParam>,
    pub body: &'a HirBlock,
}

// works for flat types, will need to revisit later
pub fn assignable(t1: &Type, t2: &Type) -> bool {
    if *t1 == Type::Any || *t2 == Type::Any {
        return true;
    }
    t1 == t2
}

pub fn type_expr(ctx: &mut PassContext, expr: &HirExpr) -> Type {
    match &expr.kind {
        // builtin
        HirExprKind::String(_) => Type::Named(ctx.builtin_types.string),
        HirExprKind::Int(_) => Type::Named(ctx.builtin_types.int),
        HirExprKind::Bool(_) => Type::Named(ctx.builtin_types.bool),
        HirExprKind::Nil => Type::Named(ctx.builtin_types.nil),

        //
        HirExprKind::VarRef { def: def_id } => {
            // should use something like ret: Rc<Type> so cloning is cheap, or
            // make this entire function return a reference, but that's for the future
            ctx.def_types[def_id.0].clone()
        },

        HirExprKind::Binary { lhs, rhs, op } => {
            let lhs_ty = type_expr(ctx, &lhs);
            let rhs_ty = type_expr(ctx, &rhs);

            // start by just checking if the types are the same
            if lhs_ty != rhs_ty {
                let msg = format!(
                    "invalid operands to binary operator '{}': '{}' and '{}",
                    Operator::describe(op),
                    fmt_type(ctx, &lhs_ty),
                    fmt_type(ctx, &rhs_ty)
                );

                // ideally in the future we should add a note/label to the expressions
                // explaining what they evaluate to, so it's clearer
                ctx.diags.push(Diagnostic::error(msg, expr.span));
                Type::Error
            } else {
                lhs_ty
            }
        },

        HirExprKind::Assign { target, value } => {
            let t1 = type_expr(ctx, &target);
            let t2 = type_expr(ctx, &value);

            if !assignable(&t1, &t2) {
                let msg = format!(
                    "cannot assign '{}' to '{}'",
                    fmt_type(ctx, &t1),
                    fmt_type(ctx, &t2)
                );
                ctx.diags.push(Diagnostic::error(msg, expr.span));
            }
            t1
        },

        HirExprKind::Call { callee, args, .. } => {
            let ty = type_expr(ctx, &callee);
            match ty {
                Type::Func { .. } => ty,
                _ => {
                    let msg = format!(
                        "attempted to call a non-function value (type: '{}')",
                        fmt_type(ctx, &ty),
                    );
                    ctx.diags.push(Diagnostic::error(msg, expr.span));
                    ty
                },
            }
        },

        HirExprKind::Func { .. } => {
            Type::Func { params: Vec::new(), ret: Box::new(Type::Any) }
        },

        HirExprKind::Field { base, .. } => {
            let base_ty = type_expr(ctx, &base);
            if base_ty == Type::Error { Type::Error } else { base_ty }
        },

        HirExprKind::Index { base, index } => {
            let base_ty = type_expr(ctx, &base);
            let index_ty = type_expr(ctx, &index);
            if base_ty == Type::Error || index_ty == Type::Error { Type::Error } else { base_ty }
        },

        //
        _ => {
            println!("expr kind not handled: '{:?}'", expr.kind);
            Type::Error
        }
    }
}

// converts an identifier type to a type enum member, so we get
// int -> Type::Int
pub fn lower_type_ref(ctx: &mut PassContext, type_ref: &TypeRef) -> (Type, Span) {
    match type_ref {
        TypeRef::Named(name, span) => {
            let symbol = ctx.interner.intern(name);
            let type_id = ctx.type_bindings.get(&symbol);

            let ty = match type_id {
                Some(id) => Type::Named(*id),
                _ => Type::Error,
            };
            (ty, *span)
        },
    }
}

pub fn fmt_type(ctx: &mut PassContext, ty: &Type) -> String {
    match ty {
        Type::Func { .. } => "fn(...) -> ...".to_string(),
        Type::Any => "any".to_string(),
        Type::Error => "<error>".to_string(),
        Type::Named(id) => {
            let type_def = &ctx.type_defs[id.0];
            // unknown should in theory never happen if the interner is correct
            ctx.interner.resolve(type_def.name).unwrap_or("<unknown>").to_string()
        },
    }
}
