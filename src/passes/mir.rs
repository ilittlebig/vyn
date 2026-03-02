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
pub struct Builder {
    pub program: MirProgram,
    pub current_func: FuncId,
    pub current_block: BlockId,
}

#[derive(Debug)]
pub struct MirProgram {
    pub entry: FuncId,
    pub funcs: Vec<MirFunction>,
}

#[derive(Debug)]
pub struct MirFunction {
    pub name: Symbol,
    pub params: Vec<LocalId>,
    pub blocks: Vec<BasicBlock>,
    pub locals: usize,
}

#[derive(Debug)]
pub struct BasicBlock {
    pub stmts: Vec<MirStmt>,
    pub term: Option<MirTerm>,
}

#[derive(Debug)]
pub enum MirStmt {
    Assign { dst: LocalId, src: MirValue },
    BinOp { dst: LocalId, op: BinOp, lhs: MirValue, rhs: MirValue },
}

#[derive(Debug)]
pub enum MirTerm {
    Goto(BlockId),
    If { cond: MirValue, then_bb: BlockId, else_bb: BlockId },
    Return(MirValue),
}

#[derive(Debug)]
pub enum MirValue {
    Local(LocalId),
    ConstInt(i64),
    ConstBool(bool),
    Nil,
}

#[derive(Debug)]
pub enum BinOp {
    Add,
}

/*
program {
  entry: fn0

  fn0 __module_init() -> nil {
    bb0:
      return nil
  }

  fn1 foo() -> int {
    bb0:
      return const 1
  }
}
*/
