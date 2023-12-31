use crate::{
    dialect::SystemRDialect,
    feedback::{ErrorKind, SystemRFeedback},
    syntax::{Token, TokenKind},
    terms::{Kind, Term},
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

/// PS05
pub fn err_05_once_failure<TExtDialect: SystemRDialect>(
    span: &Span,
    tok: &Token<TExtDialect::TokenKind>,
    message: String,
    error_msg: String,
) -> SystemRFeedback<TExtDialect> {
    SystemRFeedback::parser_error(
        "PS05",
        *span,
        tok.clone(),
        ErrorKind::ExtendedError(format!("error msg in once: {:?}", error_msg.to_owned())),
        Some(format!("message: {} error_msg: {}", message, error_msg)),
    )
}

/// PS06
pub fn err_06_expected_token<TExtDialect: SystemRDialect + 'static>(
    span: &Span,
    tok: &Token<TExtDialect::TokenKind>,
    kind: &TokenKind<TExtDialect::TokenKind>,
) -> SystemRFeedback<TExtDialect> {
    SystemRFeedback::parser_error(
        "PS06",
        *span,
        tok.clone(),
        ErrorKind::ExpectedToken(kind.clone()),
        Some(format!("expected token {:?}, found {:?}", kind, tok.kind)),
    )
}

/// PS07
pub fn err_07<TExtDialect: SystemRDialect + 'static>(
    span: &Span,
    tok: &Token<TExtDialect::TokenKind>,
    kind: &TokenKind<TExtDialect::TokenKind>,
) -> SystemRFeedback<TExtDialect> {
    SystemRFeedback::parser_error(
        "PS07",
        *span,
        tok.clone(),
        ErrorKind::ExpectedIdent,
        Some(format!("expected identifier after lambda, got {:?} instead", kind)),
    )
}

/// PS08
pub fn err_08<TExtDialect: SystemRDialect + 'static>(
    span: &Span,
    tok: &Token<TExtDialect::TokenKind>,
    kind: &TokenKind<TExtDialect::TokenKind>,
    is_upper: bool,
) -> SystemRFeedback<TExtDialect> {
    let id_type = if is_upper { "uppercase" } else { "lowercase" };
    SystemRFeedback::parser_error(
        "PS08",
        *span,
        tok.clone(),
        ErrorKind::ExpectedIdent,
        Some(format!("expected {} identifier, got {:?} instead", id_type, kind)),
    )
}

/// PS09
pub fn err_09<TExtDialect: SystemRDialect + 'static>(
    span: &Span,
    tok: &Token<TExtDialect::TokenKind>,
    var: String,
) -> SystemRFeedback<TExtDialect> {
    SystemRFeedback::parser_error(
        "PS09",
        *span,
        tok.clone(),
        ErrorKind::UnboundTypeVar,
        Some(format!("parser: unbound variable {}", var)),
    )
}

/// PS010
pub fn err_010<TExtDialect: SystemRDialect + 'static>(
    span: &Span,
    tok: &Token<TExtDialect::TokenKind>,
    atom: &Term<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    SystemRFeedback::parser_error(
        "PS010",
        *span,
        tok.clone(),
        ErrorKind::ExpectedToken(TokenKind::Proj),
        Some(format!("expected integer index after {}", atom)),
    )
}
