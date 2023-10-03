use core::fmt;

use crate::{patterns::ExtPattern, terms::ExtTerm, types::Type, syntax::{parser::{Error, ExtParser}, ExtToken, lexer2::{Lexer, ExtLexer}}, diagnostics::Diagnostic};

pub mod struct_data;

#[derive(Default)]
pub enum ParserOp<
    TExtTokenKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    TExtKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    TExtPat: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
> {
    #[default]
    Unit,
    Panic,
    Foo(TExtTokenKind, TExtKind, TExtPat)
}

pub trait SystemRExtension<
TExtTokenKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    TExtKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
    TExtPat: fmt::Debug + PartialEq + PartialOrd + Default + Clone>
{
    fn lex_is_ext_single(&self, x: char) -> bool;
    fn lex_is_extended_single_pred(&self, x: char) -> bool;
    fn lex_is_ext_keyword(&self, data: &str) -> bool;
    fn lex_extended_single(&mut self, data: &str) -> TExtTokenKind;
    fn lex_ext_keyword(&mut self, data: &str) -> TExtTokenKind;
    fn parser_has_ext_parse(&self, tk: &TExtTokenKind) -> bool;
    fn parser_ext_parse<'s>(&mut self, parser: &mut ExtParser<'s, TExtTokenKind, TExtKind, TExtPat, Self>) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>>;
    fn parser_has_ext_atom(&self, tk: &TExtTokenKind) -> bool;
    fn parser_ext_atom<'s>(&mut self, parser: &mut ExtParser<'s, TExtTokenKind, TExtKind, TExtPat, Self>) -> Result<ExtTerm<TExtPat, TExtKind>, Error<TExtTokenKind>>;
    fn pat_ext_pattern_type_eq(&self, pat: &TExtPat, ty: &Type) -> bool;
    fn pat_add_ext_pattern<'a>(
        &'a self,
        parent: &crate::types::patterns::Matrix<'a, TExtPat>,
        ext_pattern: &ExtPattern<TExtPat>,
    ) -> bool;
    fn pat_ext_matches(&self, pat: &TExtPat, term: &ExtTerm<TExtPat, TExtKind>) -> bool;
}

pub struct ParserOpCompletion<
TExtTokenKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
TExtKind: fmt::Debug + PartialEq + PartialOrd + Default + Clone,
TExtPat: fmt::Debug + PartialEq + PartialOrd + Default + Clone
>(pub String, pub ExtToken<TExtTokenKind>, pub ExtTerm<TExtPat, TExtKind>);