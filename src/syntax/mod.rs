//! Extended Lexical analysis and recursive descent parser for System F
pub mod debruijn;
pub mod error;
pub mod lexer;
pub mod parser;
use crate::{system_r_util::span::Span, dialect::ExtendedTokenKind};

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub enum TokenKind<TExtTokenKind: PartialEq> {
    Uppercase(String),
    Lowercase(String),
    Nat(u64),
    TyNat,
    TyBool,
    TyArrow,
    TyUnit,
    #[default]
    Unit,
    True,
    False,
    Lambda,
    Forall,
    Exists,
    As,
    Pack,
    Unpack,
    Succ,
    Pred,
    If,
    Then,
    Else,
    Let,
    In,
    IsZero,
    Semicolon,
    Colon,
    Comma,
    Proj,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LSquare,
    RSquare,
    Equals,
    Bar,
    Wildcard,
    Gt,
    Case,
    Of,
    Fix,
    Fold,
    Unfold,
    Rec,
    Invalid(char),
    Dummy,
    Eof,
    Tag(String),
    Extended(TExtTokenKind),
}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub struct Token<TExtTokenKind: ExtendedTokenKind> {
    pub kind: TokenKind<TExtTokenKind>,
    pub span: Span,
}

impl<TExtTokenKind: ExtendedTokenKind> Token<TExtTokenKind> {
    pub const fn dummy() -> Token<TExtTokenKind> {
        Token {
            kind: TokenKind::Dummy,
            span: Span::zero(),
        }
    }

    pub const fn new(kind: TokenKind<TExtTokenKind>, span: Span) -> Token<TExtTokenKind> {
        Token { kind, span }
    }
}
