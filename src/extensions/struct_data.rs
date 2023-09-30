use std::{rc::Rc, cell::RefCell};

use crate::{
    bottom::{BottomKind, BottomPattern, BottomTokenKind},
    platform_bindings::PlatformBindings,
    syntax::{parser::{ExtParser, Error, ErrorKind}, ExtTokenKind},
    types::{ExtContext, Type}, terms::{ExtTerm, ExtKind},
    diagnostics::Diagnostic, system_r_util::span::Span, patterns::PatVarStack,
};

use super::{SystemRExtension, ParserOp, ParserOpCompletion};

#[derive(Clone, Debug, Default)]
pub struct StructDataExtension;

impl StructDataExtension {
    fn struct_data_bind<'s>(&mut self, parser: &mut StructDataParser<'s>) -> Result<ExtTerm<StructDataPattern, StructDataKind>, Error<StructDataTokenKind>> {
        // struct decl contents
        let tyvar = parser.uppercase_id()?;
        let sp = parser.span;
        let ty = Box::new(Type::Var(parser.tyvar.push(tyvar)));
        let body = parser.once(|p| p.parse(), "abstraction body required")?;
        Ok(ExtTerm::new(ExtKind::TyAbs(Box::new(body)), sp + parser.span))
    }
}

pub type StructDataContext = ExtContext<StructDataTokenKind, StructDataKind, StructDataPattern, StructDataExtension>;

pub type StructDataParser<'s> = ExtParser<'s, StructDataTokenKind, StructDataKind, StructDataPattern, StructDataExtension>;

impl<'s> StructDataParser<'s> {
    pub fn new(platform_bindings: &'s PlatformBindings, input: &'s str, ty_let: Rc<RefCell<StructDataExtension>>) -> StructDataParser<'s> {
        ExtParser::ext_new(platform_bindings, input, ty_let)
    }
}

/// Extension 1: StructData
/// - Extends the Bottom-dialect of system_r (ie system_f with some minor
///   kind/type enhancement)
#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub enum StructDataTokenKind {
    #[default]
    Placeholder,
    StructData,
    Below(BottomTokenKind),
}

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub enum StructDataKind {
    #[default]
    Empty,
    StructIdent(String),
    /// fields: struct identifier with leading $, type shape, struct scope body (in let-polymorphism style)
    StructDataExpr(String, Box<ExtTerm<StructDataPattern, Self>>, Box<ExtTerm<StructDataPattern, Self>>),
    Below(BottomKind),
}

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub enum StructDataPattern {
    #[default]
    Empty,
    Below(BottomPattern),
}

pub const KEYWORD_TYPE: &str = "type";

impl SystemRExtension<StructDataTokenKind, StructDataKind, StructDataPattern> for StructDataExtension {
    fn lex_is_ext_single(&self, x: char) -> bool {
        false
    }

    fn lex_is_extended_single_pred(&self, x: char) -> bool {
        false
    }

    fn lex_is_ext_keyword(&self, data: &str) -> bool {
        data == KEYWORD_TYPE
    }

    fn lex_extended_single(&mut self, data: &str) -> StructDataTokenKind {
        StructDataTokenKind::Placeholder
    }

    fn lex_ext_keyword(&mut self, data: &str) -> StructDataTokenKind {
        if data == KEYWORD_TYPE {
            return StructDataTokenKind::StructData;
        }
        panic!("called lex_ext_keyword with a data str that wasn't StructData; shouldn't happen");
    }

    fn pat_ext_pattern_type_eq(&self, pat: &StructDataPattern, ty: &crate::types::Type) -> bool {
        false
    }

    fn pat_add_ext_pattern<'a>(
        &'a self,
        parent: &crate::types::patterns::Matrix<'a, StructDataPattern>,
        ext_pattern: &crate::patterns::ExtPattern<StructDataPattern>,
    ) -> bool {
        false
    }

    fn pat_ext_matches(&self, pat: &StructDataPattern, term: &crate::terms::ExtTerm<StructDataPattern, StructDataKind>) -> bool {
        false
    }

    fn parser_has_ext_parse(&self, tk: &StructDataTokenKind) -> bool {
        tk == &StructDataTokenKind::StructData
    }

    fn parser_ext_parse<'s>(&mut self, parser: &mut StructDataParser<'s>) -> Result<ExtTerm<StructDataPattern, StructDataKind>, Error<StructDataTokenKind>> {
        let sp = parser.span;
        parser.expect(ExtTokenKind::Extended(StructDataTokenKind::StructData))?;

        let struct_ident = parser.once(|p| p.ext_atom(), "missing struct identifier $Binding")?;
        let struct_ident = match struct_ident.kind {
            ExtKind::Extended(StructDataKind::StructIdent(s)) => s,
            e => return Err(Error { span: parser.span, tok: parser.token.clone(), kind: ErrorKind::ExtendedError(format!("expected a StructIdent, got {:?}", e))})
        };

        parser.expect(ExtTokenKind::Equals)?;

        //let new_struct_data = parser.once(|p| p.ty(), "type annotation required in struct data decl")?;
        let struct_shape = self.struct_data_bind(parser)?;

        let len = parser.tmvar.len();
        //parser.tmvar.push(var);
        parser.expect(ExtTokenKind::In)?;
        let t2 = parser.once(|p| p.parse(), "struct body required")?;
        while parser.tmvar.len() > len {
            parser.tmvar.pop();
        }
        Ok(ExtTerm::new(
            ExtKind::Extended(StructDataKind::StructDataExpr(struct_ident, Box::new(struct_shape), Box::new(t2))),
            sp + parser.span,
        ))
    }

    fn parser_has_ext_atom(&self, tk: &StructDataTokenKind) -> bool {
        false
    }

    fn parser_ext_atom<'s>(&mut self, parser: &mut ExtParser<'s, StructDataTokenKind, StructDataKind, StructDataPattern, Self>) -> Result<ExtTerm<StructDataPattern, StructDataKind>, Error<StructDataTokenKind>> {
        todo!()
    }
}
