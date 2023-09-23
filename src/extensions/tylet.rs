use std::{rc::Rc, cell::RefCell};

use crate::{
    bottom::{BottomKind, BottomPattern, BottomTokenKind},
    platform_bindings::PlatformBindings,
    syntax::parser::ExtParser,
    types::ExtContext, terms::{ExtTerm, ExtKind},
    diagnostics::Diagnostic, system_r_util::span::Span,
};

use super::{SystemRExtension, ParserOp, ParserOpCompletion};

#[derive(Clone, Debug, Default)]
pub struct TyLetExtension {}

pub type TyLetContext = ExtContext<TyLetTokenKind, TyLetKind, TyLetPattern, TyLetExtension>;

pub type TyLetParser<'s> = ExtParser<'s, TyLetTokenKind, TyLetKind, TyLetPattern, TyLetExtension>;

impl<'s> TyLetParser<'s> {
    pub fn new(platform_bindings: &'s PlatformBindings, input: &'s str, ty_let: Rc<RefCell<TyLetExtension>>) -> TyLetParser<'s> {
        ExtParser::ext_new(platform_bindings, input, ty_let)
    }
}

/// Extension 1: TyLet
/// - Extends the Bottom-dialect of system_r (ie system_f with some minor
///   kind/type enhancement)
#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub enum TyLetTokenKind {
    #[default]
    Placeholder,
    TyLet,
    Below(BottomTokenKind),
}

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub enum TyLetKind {
    #[default]
    Empty,
    Below(BottomKind),
}

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub enum TyLetPattern {
    #[default]
    Empty,
    Below(BottomPattern),
}

pub const TYLET_KW: &str = "tylet";

impl SystemRExtension<TyLetTokenKind, TyLetKind, TyLetPattern> for TyLetExtension {
    fn lex_is_ext_single(&self, x: char) -> bool {
        false
    }

    fn lex_is_extended_single_pred(&self, x: char) -> bool {
        false
    }

    fn lex_is_ext_keyword(&self, data: &str) -> bool {
        data == TYLET_KW
    }

    fn lex_extended_single(&mut self, data: &str) -> TyLetTokenKind {
        TyLetTokenKind::Placeholder
    }

    fn lex_ext_keyword(&mut self, data: &str) -> TyLetTokenKind {
        if data == TYLET_KW {
            return TyLetTokenKind::TyLet;
        }
        panic!("called lex_ext_keyword with a data str that wasn't tylet; shouldn't happen");
    }

    fn pat_ext_pattern_type_eq(&self, pat: &TyLetPattern, ty: &crate::types::Type) -> bool {
        false
    }

    fn pat_add_ext_pattern<'a>(
        &'a self,
        parent: &crate::types::patterns::Matrix<'a, TyLetPattern>,
        ext_pattern: &crate::patterns::ExtPattern<TyLetPattern>,
    ) -> bool {
        false
    }

    fn pat_ext_matches(&self, pat: &TyLetPattern, term: &crate::terms::ExtTerm<TyLetPattern, TyLetKind>) -> bool {
        false
    }

    fn parser_has_top_level_ext(&self, tk: &TyLetTokenKind) -> bool {
        tk == &TyLetTokenKind::TyLet
    }

    fn parser_top_level_ext<'s>(&mut self, c: ParserOpCompletion<TyLetTokenKind, TyLetKind, TyLetPattern>) -> Result<ParserOpCompletion<TyLetTokenKind, TyLetKind, TyLetPattern>, Diagnostic> {
        Err(Diagnostic::error(Span::default(), "tylet parser top level ext unimpl"))
    }
}
