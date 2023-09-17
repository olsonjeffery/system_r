use core::fmt;

use crate::{types::Type, terms::ExtTerm, patterns::ExtPattern, syntax::parser::Error};

pub mod tylet;

pub trait SystemRExtension<TExtTokenKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
        TExtKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
        TExtPat: fmt::Debug + PartialEq + PartialOrd + Default + Clone> {
    fn lex_is_ext_single(&self, x: char) -> bool;
    fn lex_is_extended_single_pred(&self, x: char) -> bool;
    fn lex_is_ext_keyword(&self, data: &str) -> bool;
    fn lex_extended_single(&mut self, data: &str) -> TExtTokenKind;
    fn lex_ext_keyword(&mut self, data: &str) -> TExtTokenKind;
    fn pat_ext_pattern_type_eq(&self, pat: &TExtPat, ty: &Type) -> bool;
    fn pat_add_ext_pattern<'a>(&'a self, parent: &crate::types::patterns::Matrix<'a, TExtPat>, ext_pattern: &ExtPattern<TExtPat>) -> bool;
    fn pat_ext_matches(&self, pat: &TExtPat, term: &ExtTerm<TExtPat, TExtKind>) -> bool;
}