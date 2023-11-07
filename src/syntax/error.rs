use core::fmt;

use crate::system_r_util::span::Span;

use super::{parser::ErrorKind, Token};

#[derive(Clone)]
pub struct Error<TExtTokenKind: PartialEq + Default + Sized + Clone> {
    pub span: Span,
    pub tok: Token<TExtTokenKind>,
    pub kind: ErrorKind<TExtTokenKind>,
}

impl<TExtTokenKind: Default + Sized + Clone + PartialEq + fmt::Debug> fmt::Debug for Error<TExtTokenKind> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Error")
            .field("kind", &format!("{:?}", self.kind))
            .finish()
    }
}
