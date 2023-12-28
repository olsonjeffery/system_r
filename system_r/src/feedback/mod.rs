#![allow(dead_code)]
use core::fmt;

use crate::{
    dialect::{ExtendedTokenKind, SystemRDialect},
    syntax::{Token, TokenKind},
    util::span::Span,
};

pub mod catalog;
pub mod syntax;

#[derive(Default, Debug, Clone, PartialEq, PartialOrd)]
pub enum FeedbackSeverity {
    #[default]
    Debug,
    Warning,
    Enhancement(String, Vec<(Span, String)>),
    Error(String),
}

#[derive(Default)]
pub struct SystemRFeedback<TExtDialect: SystemRDialect + 'static> {
    pub feedback_code: String,
    pub target_span: Option<Span>,
    pub phase: FeedbackPhase<TExtDialect>,
    pub severity: FeedbackSeverity,
    pub src: Option<String>,
    pub acknowledged: bool,
}

impl<TExtDialect: SystemRDialect> SystemRFeedback<TExtDialect> {
    fn parser_error(
        feedback_code: &str,
        span: Span,
        tok: Token<TExtDialect::TokenKind>,
        kind: ErrorKind<TExtDialect::TokenKind>,
        message: Option<String>,
    ) -> Self {
        SystemRFeedback {
            target_span: Some(span),
            feedback_code: feedback_code.to_owned(),
            phase: FeedbackPhase::Parse(tok, kind.clone()),
            severity: FeedbackSeverity::Error(message.unwrap_or(match kind.clone() {
                ErrorKind::ExtendedError(msg) => msg.clone(),
                _ => "unknown error".to_owned(),
            })),
            src: None,
            acknowledged: false,
        }
    }
    fn type_check_error(feedback_code: &str, span: Span, message: &str) -> Self {
        SystemRFeedback {
            target_span: Some(span),
            feedback_code: feedback_code.to_owned(),
            phase: FeedbackPhase::TypeCheck,
            severity: FeedbackSeverity::Error(message.to_owned()),
            src: None,
            acknowledged: false,
        }
    }
    pub fn error(
        feedback_code: String,
        target_span: Span,
        phase: FeedbackPhase<TExtDialect>,
        severity: FeedbackSeverity,
        src: Option<String>,
        acknowledged: bool,
    ) -> Self {
        SystemRFeedback {
            feedback_code,
            target_span: Some(target_span),
            phase,
            severity,
            src,
            acknowledged,
        }
    }

    #[must_use]
    /// Emit all remaining error message, if there are any
    pub fn emit(self) -> String {
        let mut _s = String::new();
        panic!("SystemRFeedback emit; shouldn;t be called");

        /*
        let lines = self.src.unwrap().lines().collect::<Vec<&str>>();
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
        */
    }
}

#[derive(Default, Debug)]
pub enum FeedbackPhase<TExtDialect: SystemRDialect + 'static> {
    #[default]
    Unknown,
    Lex,
    Parse(Token<TExtDialect::TokenKind>, ErrorKind<TExtDialect::TokenKind>),
    TypeCheck,
    Extended {
        dialect: String,
        phase: ExtendedPhaseContent<TExtDialect>,
    },
}

#[derive(Default, Debug)]
pub enum ExtendedPhaseContent<TExtDialect: SystemRDialect + 'static> {
    #[default]
    None,
    Named(String),
    Parse(Token<TExtDialect::TokenKind>, ErrorKind<TExtDialect::TokenKind>),
}

#[derive(Clone, Debug)]
pub enum ErrorKind<T: ExtendedTokenKind> {
    ExpectedAtom,
    ExpectedIdent,
    ExpectedType,
    ExpectedPattern,
    ExpectedToken(TokenKind<T>),
    UnboundTypeVar,
    Unknown,
    Eof,
    ExtendedError(String),
}

impl<TExtDialect: SystemRDialect> std::error::Error for SystemRFeedback<TExtDialect> {}

impl<TExtDialect: SystemRDialect> fmt::Debug for SystemRFeedback<TExtDialect> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Error")
            .field(
                "system-r-feedback",
                &format!("phase: {:?} severity: {:?}", self.phase, self.severity),
            )
            .finish()
    }
}
impl<TExtDialect: SystemRDialect> fmt::Display for SystemRFeedback<TExtDialect> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Error")
            .field(
                "system-r-feedback",
                &format!("phase: {:?} severity: {:?}", self.phase, self.severity),
            )
            .finish()
    }
}
