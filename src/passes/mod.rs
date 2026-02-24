/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-06
 **/

use std::collections::HashMap;
use crate::frontend::parser::Stmt;
use crate::passes::interner::Interner;
use crate::diagnostics::{ Span, Diagnostic };

mod interner;
mod name_resolve;

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
pub struct Symbol(usize);

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct DefId(usize);

#[derive(Debug, Copy, Clone, PartialEq)]
struct ScopeId(usize);

pub enum DefKind {
    LocalVar,
    Function,
    Param,
}

pub struct Def {
    name: Symbol,
    span: Span,
    kind: DefKind,
}

#[derive(Debug, Clone)]
pub struct Scope {
    parent: Option<ScopeId>,
    bindings: HashMap<Symbol, DefId>,
}

pub struct PassContext {
    pub interner: Interner,
    pub defs: Vec<Def>,
    pub scopes: Vec<Scope>,
    pub diags: Vec<Diagnostic>,
    pub current_scope: ScopeId,
}

impl PassContext {
    fn scope(&self, id: ScopeId) -> &Scope { &self.scopes[id.0] }
    fn scope_mut(&mut self, id: ScopeId) -> &mut Scope { &mut self.scopes[id.0] }

    fn push_scope(&mut self) -> ScopeId {
        let id = ScopeId(self.scopes.len());
        let parent = Some(self.current_scope);
        self.scopes.push(Scope { parent, bindings: HashMap::new() });
        self.current_scope = id;
        id
    }

    fn pop_scope(&mut self) {
        let scope = self.scope(self.current_scope).parent.expect("pop root scope");
        self.current_scope = scope;
    }
}

pub fn run_passes(ast: &Vec<Stmt>) {
    let mut ctx = PassContext {
        interner: Interner::new(),
        defs: Vec::new(),
        scopes: Vec::new(),
        diags: Vec::new(),
        current_scope: ScopeId(0),
    };

    // global scope
    ctx.scopes.push(Scope { parent: None, bindings: HashMap::new() });

    name_resolve::run(&mut ctx, ast);
}
