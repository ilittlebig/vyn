/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-25
 **/

use crate::diagnostics::Span;
use crate::frontend::parser::UnaryOp;
use crate::frontend::lexer::Operator;
use crate::passes::{ Symbol, Def, DefId };

#[derive(Debug)]
pub enum HirExprKind {
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
pub struct HirExpr {
    pub kind: HirExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct HirParam {
    pub name: Symbol,
    pub span: Span,
}

#[derive(Debug)]
pub struct HirFunc {
    pub body: Box<HirBlock>,
    pub params: Vec<HirParam>,
}

#[derive(Debug)]
pub struct HirBlock {
    pub stmts: Vec<HirStmt>,
    pub span: Span,
}

#[derive(Debug)]
pub enum HirStmt {
    Decl { name: Symbol, init: Option<HirExpr> },
    FuncDecl { name: Symbol, params: Vec<HirParam>, init: HirBlock },

    While { cond: HirExpr, body: HirBlock },
    If { cond: HirExpr, then_block: HirBlock, else_block: Option<HirBlock> },

    Return(Option<HirExpr>),
    Break,
    Continue,

    Block(HirBlock),
    Expr(HirExpr),
}
