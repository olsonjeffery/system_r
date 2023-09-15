use crate::{syntax::{parser::{ExtParser, ParserExtension}}, bottom::{BottomTokenKind, BottomKind, BottomPattern}, types::ExtContext, patterns::PatternExtension, platform_bindings::PlatformBindings};

use super::SystemRExtension;

#[derive(Clone, Debug, Default)]
pub struct TyLetExtension {
    
}

pub type TyLetContext = ExtContext<TyLetPattern, TyLetKind, TyLetExtension>;

pub type TyLetParser<'s> = ExtParser<'s, TyLetTokenKind, TyLetKind, TyLetPattern, TyLetExtension, TyLetExtension>;

impl<'s> TyLetParser<'s> {
    pub fn new(platform_bindings: &'s PlatformBindings, input: &'s str, ty_let: TyLetExtension) -> TyLetParser<'s> {
        ExtParser::ext_new(platform_bindings, input, ty_let.clone(), ty_let)
    }
}

/// Extension 1: TyLet
/// - Extends the Bottom-dialect of system_r (ie system_f with some minor kind/type enhancement)
#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub enum TyLetTokenKind {
    #[default]
    Placeholder,
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

impl ParserExtension<TyLetTokenKind, TyLetKind> for TyLetExtension {

}

impl SystemRExtension<TyLetTokenKind, TyLetKind, TyLetPattern> for TyLetExtension {
    fn lex_is_ext_single(&self, x: char) -> bool {
        false
    }

    fn lex_is_extended_single_pred(&self, x: char) -> bool {
        false
    }

    fn lex_is_ext_keyword(&self, data: &str) -> bool {
        false
    }

    fn lex_extended_single(&mut self, data: &str) -> TyLetTokenKind {
        TyLetTokenKind::Placeholder
    }

    fn lexer_lex_ext_keyword(&mut self, data: &str) -> TyLetTokenKind {
        TyLetTokenKind::Placeholder
    }

    fn pat_ext_pattern_type_eq(&self, pat: &TyLetPattern, ty: &crate::types::Type) -> bool {
        false
    }

    fn pat_add_ext_pattern<'a>(&'a self, parent: &crate::types::patterns::Matrix<'a, TyLetPattern, TyLetKind>, ext_pattern: &crate::patterns::ExtPattern<TyLetPattern, TyLetKind>) -> bool {
        false
    }

    fn pat_ext_matches(&self, pat: &TyLetPattern, term: &crate::terms::ExtTerm<TyLetPattern, TyLetKind>) -> bool {
        false
    }
}

impl PatternExtension<TyLetPattern, TyLetKind> for TyLetExtension {
    fn ext_pattern_type_eq(&self, pat: &TyLetPattern, ty: &crate::types::Type) -> bool {
        false
    }

    fn add_ext_pattern<'a>(&'a self, parent: &crate::types::patterns::Matrix<'a, TyLetPattern, TyLetKind>, ext_pattern: &crate::patterns::ExtPattern<TyLetPattern, TyLetKind>) -> bool {
        false
    }

    fn ext_matches(&self, pat: &TyLetPattern, term: &crate::terms::ExtTerm<TyLetPattern, TyLetKind>) -> bool {
        false
    }
}