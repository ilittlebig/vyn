/*
 * Hello World
 *
 * Author: Elias Sjödin
 * Created: 2026-03-10
 **/

use std::fmt::{ self, Write };

pub struct Printer<W: Write> {
    pub out: W,
    pub indent: usize,
}

impl<W: Write> Printer<W> {
    pub fn new(out: W) -> Self {
        Self { out, indent: 0 }
    }

    pub fn indent(&mut self) {
        let n = self.indent * 4;
        for i in 0..n {
            self.out.write_char(' ');
        }
    }

    pub fn with_indent<F: FnOnce(&mut Self)>(&mut self, f: F) {
        self.indent += 1;
        f(self);
        self.indent -= 1;
    }

    pub fn line(&mut self, s: &str) {
        self.indent();
        let _ = writeln!(&mut self.out, "{s}");
    }

    pub fn line_fmt(&mut self, args: fmt::Arguments) {
        self.indent();
        let _ = self.out.write_fmt(args);
        self.out.write_char('\n');
    }

    pub fn begin_line(&mut self) {
        self.indent();
    }

    pub fn end_line(&mut self) {
        self.out.write_char('\n');
    }

    pub fn write_raw(&mut self, s: &str) {
        let _ = self.out.write_str(s);
    }

    pub fn write_raw_fmt(&mut self, args: fmt::Arguments) {
        let _ = self.out.write_fmt(args);
    }

    // takes ownership of out
    // used if the callee wants to do something with the output
    pub fn finish(self) -> W { self.out }
}
