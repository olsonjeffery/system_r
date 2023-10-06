/*
Copyright (C) 2023 AUTHORS

GNU Lesser General Public License Version 3

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
use std::{cell::RefCell, rc::Rc};

use crate::{
    bottom::{BottomKind, BottomPattern, BottomTokenKind, BottomExtension},
    diagnostics::Diagnostic,
    patterns::PatVarStack,
    platform_bindings::PlatformBindings,
    syntax::{
        parser,
        parser::{ErrorKind, ParserState},
        error::Error,
        ExtTokenKind,
    },
    system_r_util::span::Span,
    terms::{ExtKind, ExtTerm},
    types::{ExtContext, Type},
};

use super::{ParserOp, ParserOpCompletion, SystemRExtension};

#[derive(Copy, Clone, Debug, Default)]
pub struct StructDataExtension;

pub type StructDataContext = ExtContext<StructDataTokenKind, StructDataKind, StructDataPattern, StructDataExtension>;

pub fn new<'s>(
    platform_bindings: &'s PlatformBindings,
    input: &'s str,
    ty_let: Rc<RefCell<StructDataExtension>>,
) -> ParserState<'s, StructDataTokenKind, StructDataKind, StructDataPattern> {
    parser::ext_new(platform_bindings, input, &mut StructDataExtension)
}

/// Extension 1: StructData
/// - Extends the Bottom-dialect of system_r (ie system_f with some minor
///   kind/type enhancement)
#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub enum StructDataTokenKind {
    #[default]
    Placeholder,
    StructData,
    TypeBindingVar(String),
    Below(BottomTokenKind),
}

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub enum StructDataKind {
    #[default]
    Empty,
    Declaration(Box<Type>),
    StructIdent(String),
    /// fields: struct identifier with leading $, type shape, struct scope body
    /// (in let-polymorphism style)
    StructDataExpr(
        String,
        Box<ExtTerm<StructDataPattern, Self>>,
        Box<ExtTerm<StructDataPattern, Self>>,
    ),
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
        x == '$'
    }

    fn lex_is_extended_single_pred(&self, x: char) -> bool {
        x == '$' || (!x.is_whitespace() && x.is_ascii_alphanumeric())
    }

    fn lex_is_ext_keyword(&self, data: &str) -> bool {
        data == KEYWORD_TYPE
    }

    fn lex_extended_single(&mut self, data: &str) -> StructDataTokenKind {
        StructDataTokenKind::TypeBindingVar(data.to_owned())
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

    fn pat_ext_matches(
        &self,
        pat: &StructDataPattern,
        term: &crate::terms::ExtTerm<StructDataPattern, StructDataKind>,
    ) -> bool {
        false
    }

    fn parser_has_ext_parse(&self, tk: &StructDataTokenKind) -> bool {
        tk == &StructDataTokenKind::StructData
    }

    fn parser_ext_parse<'s>(
        &mut self,
        ps: &mut ParserState<'s, StructDataTokenKind, StructDataKind, StructDataPattern>,
    ) -> Result<ExtTerm<StructDataPattern, StructDataKind>, Error<StructDataTokenKind>> {
        let sp = ps.span;
        parser::expect(ps, self, ExtTokenKind::Extended(StructDataTokenKind::StructData))?;

        let struct_ident = parser::once(ps, |p| parser::atom(p, self), "missing struct identifier $Binding")?;
        let struct_ident = match struct_ident.kind {
            ExtKind::Extended(StructDataKind::StructIdent(s)) => s,
            e => {
                return Err(Error {
                    span: ps.span,
                    tok: ps.token.clone(),
                    kind: ErrorKind::ExtendedError(format!("expected a StructIdent, got {:?}", e)),
                })
            }
        };

        parser::expect(ps, self, ExtTokenKind::Equals)?;

        //let new_struct_data = parser.once(|p| p.ty(), "type annotation required in
        // struct data decl")?;
        let struct_shape = struct_data_bind(ps, self)?;

        let len = ps.tmvar.len();
        //parser.tmvar.push(var);
        parser::expect(ps, self, ExtTokenKind::In)?;
        //panic!("about to parser type scope body, have name: {:?} have type: {:?}", struct_ident, struct_shape);
        let t2 = parser::once(ps, |p| parser::parse(p, self), "type scope body required")?;
        while ps.tmvar.len() > len {
            ps.tmvar.pop();
        }
        Ok(ExtTerm::new(
            ExtKind::Extended(StructDataKind::StructDataExpr(
                struct_ident,
                Box::new(struct_shape),
                Box::new(t2),
            )),
            sp + ps.span,
        ))
    }

    fn parser_has_ext_atom(&self, tk: &StructDataTokenKind) -> bool {
        false
    }

    fn parser_ext_atom<'s>(
        &mut self,
        ps: &mut ParserState<'s, StructDataTokenKind, StructDataKind, StructDataPattern>,
    ) -> Result<ExtTerm<StructDataPattern, StructDataKind>, Error<StructDataTokenKind>> {
        let name_tok = ps.token.clone();
        let name_val = match name_tok.kind.clone() {
            ExtTokenKind::Extended(n) => match n {
                StructDataTokenKind::TypeBindingVar(name) => name,
                v => return Err(Error {span: name_tok.span.clone(), tok: name_tok.clone(), kind: ErrorKind::ExtendedError(format!("Expected a TypeBindingVar, got {:?}", v))})
            },
            v => return Err(Error {span: name_tok.span.clone(), tok: name_tok.clone(), kind: ErrorKind::ExtendedError(format!("Expected a ExtendedTokenKind, got {:?}", v))})
        };
        parser::bump(ps, self);
        Ok(ExtTerm { span: name_tok.span.clone(), kind: ExtKind::Extended(StructDataKind::StructIdent(name_val))})
    }

    fn parser_ty_bump_if(&mut self, 
        ps: &mut ParserState<StructDataTokenKind, StructDataKind, StructDataPattern>,
    ) -> bool {
        match &ps.token.kind {
            ExtTokenKind::Extended(StructDataTokenKind::TypeBindingVar(_)) => {
                true
            },
            _ => false
        }
    }

    fn parser_ty(&mut self, 
        ps: &mut ParserState<StructDataTokenKind, StructDataKind, StructDataPattern>,
    ) -> Result<Type, Error<StructDataTokenKind>> {
        let binding = ps.token.kind.clone();
        let ExtTokenKind::Extended(StructDataTokenKind::TypeBindingVar(type_decl_key)) = binding else {
            return Err(Error {span: ps.span, tok: ps.token.clone(), kind: ErrorKind::ExtendedError(format!("Expected a TypeBindingVar, got {:?}", binding))})
        };
        parser::bump(ps, self);

        let ty = extract_type_from_tyapp(ps, self)?;

        panic!("parser_ty unimpl, type_decl_key: {:?} type: {:?}", type_decl_key, ty);
    }
}

pub fn extract_type_from_tyapp(
        ps: &mut ParserState<StructDataTokenKind, StructDataKind, StructDataPattern>,
        ext: &mut StructDataExtension,
) -> Result<Type, Error<StructDataTokenKind>> {
    if !parser::bump_if(ps, ext, &ExtTokenKind::LSquare) {
        return parser::error(ps, ErrorKind::ExpectedToken(ExtTokenKind::LSquare));
    }
    parser::expect(ps, ext, ExtTokenKind::Of)?;
    let ty = parser::ty(ps, ext)?;
    parser::expect(ps, ext, ExtTokenKind::RSquare)?;
    // FIXME sub out with what's in parser ext state
    Ok(ty)
}

pub fn struct_data_bind<'s>(
    ps: &mut ParserState<'s, StructDataTokenKind, StructDataKind, StructDataPattern>,
    ext: &mut StructDataExtension,
) -> Result<ExtTerm<StructDataPattern, StructDataKind>, Error<StructDataTokenKind>> {
    // struct decl contents
    parser::expect(ps, ext, ExtTokenKind::Lambda)?;
    let tyvar = parser::uppercase_id(ps, ext)?;

    let sp = ps.span;
    let ty = Box::new(Type::Var(ps.tyvar.push(tyvar)));
    
    let body = parser::once(ps, |p| parser::ty_atom(p, &mut StructDataExtension), "abstraction body required")?;
    let kind = ExtKind::Extended(StructDataKind::Declaration(Box::new(body)));
    Ok(ExtTerm::new(kind, sp + ps.span))
}