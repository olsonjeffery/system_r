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
use crate::system_r_util::span::Span;
#[derive(Debug, Default, Copy, Clone)]
pub enum Level {
    #[default]
    Warn,
    Error,
}

#[derive(Debug, Default, Clone)]
pub struct Annotation {
    pub span: Span,
    pub info: String,
}

#[derive(Debug, Default, Clone)]
pub struct Diagnostic {
    pub level: Level,
    pub primary: Annotation,
    pub info: Vec<String>,
    pub other: Vec<Annotation>,
}

impl Annotation {
    pub fn new<S: Into<String>>(span: Span, message: S) -> Annotation {
        Annotation {
            span,
            info: message.into(),
        }
    }
}

impl Diagnostic {
    pub fn error<S: Into<String>>(span: Span, message: S) -> Diagnostic {
        Diagnostic {
            level: Level::Error,
            primary: Annotation::new(span, message),
            other: Vec::new(),
            info: Vec::new(),
        }
    }

    pub fn warn<S: Into<String>>(span: Span, message: S) -> Diagnostic {
        Diagnostic {
            level: Level::Warn,
            primary: Annotation::new(span, message),
            other: Vec::new(),
            info: Vec::new(),
        }
    }

    pub fn message<S: Into<String>>(mut self, span: Span, message: S) -> Diagnostic {
        self.other.push(Annotation::new(span, message));
        self
    }

    pub fn info<S: Into<String>>(mut self, info: S) -> Diagnostic {
        self.info.push(info.into());
        self
    }

    pub fn lines(&self) -> std::ops::Range<u32> {
        let mut range = std::ops::Range {
            start: self.primary.span.start.line,
            end: self.primary.span.end.line + 1,
        };

        for addl in &self.other {
            if addl.span.start.line < range.start {
                range.start = addl.span.start.line;
            }
            if addl.span.end.line + 1 > range.end {
                range.end = addl.span.end.line + 1;
            }
        }
        range
    }
}
