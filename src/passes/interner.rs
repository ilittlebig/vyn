/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-02-06
 **/

use std::rc::Rc;
use std::collections::HashMap;
use crate::passes::Symbol;

#[derive(Debug)]
pub struct Interner {
    map: HashMap<Rc<str>, Symbol>,
    strings: Vec<Rc<str>>,
}

impl Interner {
    pub fn new() -> Self {
        Self { map: HashMap::new(), strings: Vec::new() }
    }

    pub fn intern(&mut self, ident: &str) -> Symbol {
        let rc: Rc<str> = Rc::from(ident);
        if let Some(sym) = self.map.get(ident) { return *sym; }

        self.strings.push(rc.clone());
        let id = self.strings.len() - 1;

        let symbol = Symbol(id);
        self.map.insert(rc, symbol);
        symbol
    }

    pub fn resolve(&self, symbol: Symbol) -> Option<&str> {
        match symbol {
            Symbol(i) => self.strings.get(i).map(|s| s.as_ref())
        }
    }
}
