/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-01
 **/

use crate::passes::Symbol;

#[derive(Debug, Clone, Copy)]
pub struct FuncId(pub usize);
#[derive(Debug, Clone, Copy)]
pub struct BlockId(pub usize);
#[derive(Debug, Clone, Copy)]
pub struct LocalId(pub usize);

#[derive(Debug)]
pub struct LoopContext {
    pub break_bb: Option<BlockId>,
    pub continue_bb: Option<BlockId>,
}

#[derive(Debug)]
pub struct Builder {
    pub program: MirProgram,
    pub current_func: FuncId,
    pub current_block: BlockId,
    pub loop_context: LoopContext,
    pub def_to_local: Vec<Option<LocalId>>,
    pub def_to_func: Vec<Option<FuncId>>,
}

#[derive(Debug)]
pub struct MirProgram {
    pub entry: FuncId,
    pub funcs: Vec<MirFunction>,
}

#[derive(Debug)]
pub struct MirFunction {
    pub id: FuncId,
    pub name: Symbol,
    pub params: Vec<LocalId>,
    pub blocks: Vec<BasicBlock>,
    pub locals: usize,
}

#[derive(Debug)]
pub struct BasicBlock {
    pub id: BlockId,
    pub stmts: Vec<MirStmt>,
    pub term: Option<MirTerm>,
}

#[derive(Debug)]
pub enum MirStmt {
    Assign { dst: MirPlace, src: MirValue },
    BinOp { dst: LocalId, op: BinOp, lhs: MirValue, rhs: MirValue },
    Call { dst: LocalId, callee: MirValue, args: Vec<MirValue> },
}

#[derive(Debug)]
pub enum MirTerm {
    Goto(BlockId),
    If { cond: MirValue, then_bb: BlockId, else_bb: BlockId },
    Return(MirValue),
}

#[derive(Debug, Clone)]
pub enum MirValue {
    Func(FuncId),
    Local(LocalId),
    ConstInt(i64),
    ConstDouble(f64),
    ConstBool(bool),
    Nil,
}

#[derive(Debug)]
pub enum MirPlace {
    Local(LocalId),
}

#[derive(Debug)]
pub enum BinOp {
    // arithmetic
    Add,
    Minus,
    Division,
    Multiplication,
    Modulus,

    // comparison
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}
