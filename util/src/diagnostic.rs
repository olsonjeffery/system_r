/*
Copyright (C) 2020 Micheal Lazear

The MIT License (MIT)

Copyright (c) ${license.years} ${license.owner}

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
//! Diagnostic handling for errors detected in source code.
//!
//! Dropping a [`Diagnostic`] without calling `emit` will cause a [`panic`]!
use crate::span::*;

/// Struct that handles collecting and reporting Parser errors and diagnostics
pub struct Diagnostic<'s> {
    src: &'s str,
    messages: Vec<Spanned<String>>,
}

impl Diagnostic<'_> {
    pub fn new(src: &str) -> Diagnostic<'_> {
        Diagnostic {
            src,
            messages: Vec::new(),
        }
    }

    pub fn push<S: Into<String>>(&mut self, msg: S, span: Span) {
        self.messages.push(Spanned::new(span, msg.into()));
    }

    pub fn error_count(&self) -> usize {
        self.messages.len()
    }

    /// Remove the last error message
    pub fn pop(&mut self) -> Option<String> {
        let msg = self.messages.pop()?;
        let line = self.src.lines().nth(msg.span.start.line as usize)?;
        Some(format!(
            "Error occuring at line {}, col: {}: {}\n{}\n{}^{}\n",
            msg.span.start.line,
            msg.span.start.col,
            msg.data,
            &line,
            (0..msg.span.start.col).map(|_| ' ').collect::<String>(),
            (0..msg.span.end.col - msg.span.start.col)
                .map(|_| '~')
                .collect::<String>(),
        ))
    }

    #[must_use]
    /// Emit all remaining error message, if there are any
    pub fn emit(mut self) -> String {
        let mut s = String::new();

        let lines = self.src.lines().collect::<Vec<&str>>();
        for i in 0..self.messages.len() {
            let msg: &Spanned<String> = &self.messages[i];
            let mut squiggly = (1..msg.span.end.col.saturating_sub(msg.span.start.col))
                .map(|_| '~')
                .collect::<String>();
            squiggly.push('^');
            s.push_str(&format!(
                "Error occuring at line {}, col: {}: {} {} {} {}",
                msg.span.start.line,
                msg.span.start.col,
                msg.data,
                &lines[msg.span.start.line as usize],
                (0..msg.span.start.col).map(|_| ' ').collect::<String>(),
                squiggly
            ));
        }
        self.messages.clear();
        s
    }
}

impl Drop for Diagnostic<'_> {
    fn drop(&mut self) {
        if self.error_count() != 0 {
            panic!("Diagnostic dropped without handling {} errors!", self.error_count());
        }
    }
}
