use core::fmt;

use crate::{dialect::ExtendedTokenKind, system_r_util::span::Span};

use super::{parser::ErrorKind, Token};

#[derive(Clone)]
pub struct Error<TExtTokenKind: ExtendedTokenKind> {
    pub span: Span,
    pub tok: Token<TExtTokenKind>,
    pub kind: ErrorKind<TExtTokenKind>,
}

impl<T: ExtendedTokenKind + fmt::Debug + fmt::Display> std::error::Error for Error<T> {}

impl<TExtTokenKind: ExtendedTokenKind> fmt::Debug for Error<TExtTokenKind> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Error")
            .field("kind", &format!("{:?}", self.kind))
            .finish()
    }
}
impl<TExtTokenKind: ExtendedTokenKind> fmt::Display for Error<TExtTokenKind> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Error")
            .field("kind", &format!("{:?}", self.kind))
            .finish()
    }
}
