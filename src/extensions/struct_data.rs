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
use std::{cell::RefCell, rc::Rc, collections::HashMap};

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

pub type StructDataContext = ExtContext<StructDataTokenKind, StructDataKind, StructDataPattern>;

pub fn new<'s>(
    platform_bindings: &'s PlatformBindings,
    input: &'s str,
    ty_let: Rc<RefCell<StructDataExtension>>,
) -> ParserState<'s, StructDataTokenKind, StructDataKind, StructDataPattern, StructDataState> {
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

#[derive(Clone, Default, Debug)]
pub struct StructDataState {
    type_map: HashMap<String, Type>,
}

pub const KEYWORD_TYPE: &str = "type";

impl SystemRExtension<StructDataTokenKind, StructDataKind, StructDataPattern, StructDataState> for StructDataExtension {
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
        ps: &mut ParserState<'s, StructDataTokenKind, StructDataKind, StructDataPattern, StructDataState>,
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

        // this should be a "holed-out" type specification (it should have
        // entries for where generic types can be substituted-in)
        let type_shape = get_type_from_decl(ps, self)?;
        // type_shape should be stored onto the ps.ext_state.type_map

        let len = ps.tmvar.len();
        parser::expect(ps, self, ExtTokenKind::In)?;
        let t2 = parser::once(ps, |p| parser::parse(p, self), "type scope body required")?;
        while ps.tmvar.len() > len {
            ps.tmvar.pop();
        }

        // FIXME: Maybe this should just be returning t2, since its contents should have
        // erased all type/extended token/kind stuff by now
        Ok(ExtTerm::new(
            ExtKind::Extended(StructDataKind::StructDataExpr(
                struct_ident,
                Box::new(type_shape),
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
        ps: &mut ParserState<'s, StructDataTokenKind, StructDataKind, StructDataPattern, StructDataState>,
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
        ps: &mut ParserState<StructDataTokenKind, StructDataKind, StructDataPattern, StructDataState>,
    ) -> bool {
        match &ps.token.kind {
            ExtTokenKind::Extended(StructDataTokenKind::TypeBindingVar(_)) => {
                true
            },
            _ => false
        }
    }

    fn parser_ty(&mut self, 
        ps: &mut ParserState<StructDataTokenKind, StructDataKind, StructDataPattern, StructDataState>,
    ) -> Result<Type, Error<StructDataTokenKind>> {
        let binding = ps.token.kind.clone();

        let ExtTokenKind::Extended(StructDataTokenKind::TypeBindingVar(type_decl_key)) = binding else {
            return Err(Error {span: ps.span, tok: ps.token.clone(), kind: ErrorKind::ExtendedError(format!("Expected a TypeBindingVar, got {:?}", binding))})
        };
        parser::bump(ps, self);

        // FIXME need to peek ahead and see if there's actually a tyapp after the use of our
        // TypeBindingVar
        let ty = extract_fulfilled_type_alias_from_tyapp(ps, self, &type_decl_key)?;

        panic!("parser_ty unimpl, type_decl_key: {:?} type: {:?}", type_decl_key, ty);

        // FIXME ultimately we are returning a fully substituted type with no "type holes"
    }
}

pub fn extract_fulfilled_type_alias_from_tyapp(
        ps: &mut ParserState<StructDataTokenKind, StructDataKind, StructDataPattern, StructDataState>,
        ext: &mut StructDataExtension,
        key: &str,
) -> Result<Type, Error<StructDataTokenKind>> {
    // FIXME: could be 1 or more
    if !parser::bump_if(ps, ext, &ExtTokenKind::LSquare) {
        return parser::error(ps, ErrorKind::ExpectedToken(ExtTokenKind::LSquare));
    }
    parser::expect(ps, ext, ExtTokenKind::Of)?;
    let ty = parser::ty(ps, ext)?;
    parser::expect(ps, ext, ExtTokenKind::RSquare)?;
    // FIXME sub out with what's in parser ext state.
    // Use the key param above to pull out the stored type_shape,
    // apply tyapps from above, then substitute in-place
    Ok(ty)
}

pub fn get_type_from_decl<'s>(
    ps: &mut ParserState<'s, StructDataTokenKind, StructDataKind, StructDataPattern, StructDataState>,
    ext: &mut StructDataExtension,
) -> Result<ExtTerm<StructDataPattern, StructDataKind>, Error<StructDataTokenKind>> {
    // tyabs -- FIXME need to peek and peel one or more off
    parser::expect(ps, ext, ExtTokenKind::Lambda)?;
    let tyvar = parser::uppercase_id(ps, ext)?;
    let sp = ps.span;
    let tyabs = Box::new(Type::Var(ps.tyvar.push(tyvar)));
    
    // type def
    let type_shape = parser::once(ps, |p| parser::ty_atom(p, &mut StructDataExtension), "abstraction body required")?;
    // FIXME scrub all occurance of tyabs's from above type_shape, replaced
    // with indexed "type holes", need to extend Type
    let kind = ExtKind::Extended(StructDataKind::Declaration(Box::new(type_shape.clone())));

    // pop off the tyabs var, since they only apply for the purpose of defining
    // "type holes" in the type def
    // FIXME do number of pops equiv to pushes above, based on number of tyvar
    ps.tyvar.pop();


    Ok(ExtTerm::new(kind, sp + ps.span))
}