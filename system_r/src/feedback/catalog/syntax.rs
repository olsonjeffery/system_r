use crate::{
    dialect::SystemRDialect,
    feedback::{ErrorKind, SystemRFeedback},
    syntax::Token,
    terms::Kind,
    util::span::Span,
};

/// PS01
pub fn err_01<TExtDialect: SystemRDialect>(
    span: Span,
    tok: Token<TExtDialect::TokenKind>,
    kind: ErrorKind<TExtDialect::TokenKind>,
) -> SystemRFeedback<TExtDialect> {
    SystemRFeedback::parser_error("PS01", span, tok, kind, None)
}

/// PS02 -- generic .error() on Parser
pub fn err_02<TExtDialect: SystemRDialect>(
    span: Span,
    tok: Token<TExtDialect::TokenKind>,
    kind: ErrorKind<TExtDialect::TokenKind>,
) -> SystemRFeedback<TExtDialect> {
    SystemRFeedback::parser_error("PS02", span, tok, kind, None)
}

/// PS03
pub fn err_03<TExtDialect: SystemRDialect>(
    span: Span,
    tok: Token<TExtDialect::TokenKind>,
    kind: ErrorKind<TExtDialect::TokenKind>,
    v: Kind<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    SystemRFeedback::parser_error(
        "PS03",
        span,
        tok,
        kind,
        Some(format!(r#" expect Nat literals from 0 to 255, got {v:?}; invalid byte range "#).to_owned()),
    )
}

/// PS04
pub fn err_04<TExtDialect: SystemRDialect>(
    span: Span,
    tok: Token<TExtDialect::TokenKind>,
    kind: ErrorKind<TExtDialect::TokenKind>,
    v: Kind<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    SystemRFeedback::parser_error(
        "PS04",
        span,
        tok,
        kind,
        Some(format!(r#" expect Nat literals from 0 to 255, got {v:?}; invalid byte range "#).to_owned()),
    )
}
