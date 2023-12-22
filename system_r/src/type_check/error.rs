use std::{error::Error, fmt::Display};

use crate::{util::span::Span, dialect::SystemRDialect};

use super::Type;
#[derive(Debug, Default, Copy, Clone)]
pub enum Level {
    #[default]
    Warn,
    Error,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct TypeError<TExtDialect: SystemRDialect> {
    pub span: Span,
    pub kind: TypeErrorKind<TExtDialect>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum TypeErrorKind<TExtDialect: SystemRDialect> {
    ParameterMismatch(Box<Type<TExtDialect>>, Box<Type<TExtDialect>>, Span),
    InvalidProjection,
    NotArrow,
    NotUniversal,
    NotVariant,
    NotProduct,
    NotRec,
    IncompatibleArms,
    InvalidPattern,
    NotExhaustive,
    UnreachablePattern,
    UnboundVariable(usize),
}

#[derive(Debug, Default, Clone)]
pub struct Annotation {
    pub span: Span,
    pub info: String,
}

#[derive(Debug, Default, Clone)]
pub struct TypeCheckerDiagnosticInfo {
    pub level: Level,
    pub primary: Annotation,
    pub info: Vec<String>,
    pub other: Vec<Annotation>,
}

impl Error for TypeCheckerDiagnosticInfo {}

impl Display for TypeCheckerDiagnosticInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Diagnostic({:?})", self.level)
    }
}

impl Annotation {
    pub fn new<S: Into<String>>(span: Span, message: S) -> Annotation {
        Annotation {
            span,
            info: message.into(),
        }
    }
}

impl TypeCheckerDiagnosticInfo {
    pub fn error<S: Into<String>>(span: Span, message: S) -> TypeCheckerDiagnosticInfo {
        TypeCheckerDiagnosticInfo {
            level: Level::Error,
            primary: Annotation::new(span, message),
            other: Vec::new(),
            info: Vec::new(),
        }
    }

    pub fn warn<S: Into<String>>(span: Span, message: S) -> TypeCheckerDiagnosticInfo {
        TypeCheckerDiagnosticInfo {
            level: Level::Warn,
            primary: Annotation::new(span, message),
            other: Vec::new(),
            info: Vec::new(),
        }
    }

    pub fn message<S: Into<String>>(mut self, span: Span, message: S) -> TypeCheckerDiagnosticInfo {
        self.other.push(Annotation::new(span, message));
        self
    }

    pub fn info<S: Into<String>>(mut self, info: S) -> TypeCheckerDiagnosticInfo {
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
