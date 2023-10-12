/*
Copyright (C) 2023 AUTHORS

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License in the ./LICENSE.APACHE2 file
in this repository.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
use std::{cell::RefCell, collections::HashMap, ops::Range, rc::Rc};

use crate::{
    bottom::{BottomExtension, BottomKind, BottomPattern, BottomTokenKind, BottomType},
    diagnostics::Diagnostic,
    patterns::PatVarStack,
    platform_bindings::PlatformBindings,
    syntax::{
        error::Error,
        parser,
        parser::{ErrorKind, ParserState},
        ExtTokenKind,
    },
    terms::{ExtKind, ExtTerm},
    types::{visit::Subst, ExtContext, Type},
    visit::MutTypeVisitor,
};

use super::{SystemRTranslator, SystemRExtension};

#[derive(Copy, Clone, Debug, Default)]
pub struct StructDataExtension;

pub type StructDataContext = ExtContext<StructDataTokenKind, StructDataKind, StructDataPattern, StructDataType>;

pub fn new<'s>(
    platform_bindings: &'s PlatformBindings,
    input: &'s str,
    ty_let: Rc<RefCell<StructDataExtension>>,
) -> ParserState<'s, StructDataTokenKind, StructDataKind, StructDataPattern, StructDataType, StructDataState> {
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
    Declaration(Box<Type<StructDataType>>),
    StructIdent(String),
    /// fields: struct identifier with leading $, type shape, struct scope body
    /// (in let-polymorphism style)
    StructDataExpr(
        String,
        Box<ExtTerm<StructDataPattern, Self, StructDataType>>,
        Box<ExtTerm<StructDataPattern, Self, StructDataType>>,
    ),
    Below(BottomKind),
}

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub enum StructDataPattern {
    #[default]
    Empty,
    Below(BottomPattern),
}

// TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq,
#[derive(Clone, Debug, Default, PartialEq, PartialOrd, Eq, Hash)]
pub enum StructDataType {
    #[default]
    Empty,
    TypeAliasApp(String, Vec<Type<StructDataType>>),
}

#[derive(Clone, Default, Debug)]
pub struct StructDataState {
    type_map: HashMap<String, (usize, Type<StructDataType>)>,
}

pub const KEYWORD_TYPE: &str = "type";

impl SystemRExtension<StructDataTokenKind, StructDataKind, StructDataPattern, StructDataType, StructDataState>
    for StructDataExtension
{
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

    fn pat_ext_pattern_type_eq(&self, pat: &StructDataPattern, ty: &crate::types::Type<StructDataType>) -> bool {
        false
    }

    fn pat_add_ext_pattern<'a>(
        &'a self,
        parent: &crate::types::patterns::Matrix<'a, StructDataPattern, StructDataType>,
        ext_pattern: &crate::patterns::ExtPattern<StructDataPattern>,
    ) -> bool {
        false
    }

    fn pat_ext_matches(
        &self,
        pat: &StructDataPattern,
        term: &crate::terms::ExtTerm<StructDataPattern, StructDataKind, StructDataType>,
    ) -> bool {
        false
    }

    fn parser_has_ext_parse(&self, tk: &StructDataTokenKind) -> bool {
        tk == &StructDataTokenKind::StructData
    }

    fn parser_ext_parse<'s>(
        &mut self,
        ps: &mut ParserState<
            's,
            StructDataTokenKind,
            StructDataKind,
            StructDataPattern,
            StructDataType,
            StructDataState,
        >,
    ) -> Result<ExtTerm<StructDataPattern, StructDataKind, StructDataType>, Error<StructDataTokenKind>> {
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
        let type_shape = get_holed_type_from_decl(ps, self)?;
        set_holed_type_for(ps, &struct_ident, type_shape)?;

        let len = ps.tmvar.len();
        parser::expect(ps, self, ExtTokenKind::In)?;

        let t2 = parser::once(ps, |p| parser::parse(p, self), "type scope body required")?;
        while ps.tmvar.len() > len {
            ps.tmvar.pop();
        }

        // FIXME return the whole structure, move erasure
        // logic to resolve()
        Ok(t2)
    }

    fn parser_has_ext_atom(&self, tk: &StructDataTokenKind) -> bool {
        false
    }

    fn parser_ext_atom<'s>(
        &mut self,
        ps: &mut ParserState<
            's,
            StructDataTokenKind,
            StructDataKind,
            StructDataPattern,
            StructDataType,
            StructDataState,
        >,
    ) -> Result<ExtTerm<StructDataPattern, StructDataKind, StructDataType>, Error<StructDataTokenKind>> {
        let name_tok = ps.token.clone();
        let name_val = match name_tok.kind.clone() {
            ExtTokenKind::Extended(n) => match n {
                StructDataTokenKind::TypeBindingVar(name) => name,
                v => {
                    return Err(Error {
                        span: name_tok.span.clone(),
                        tok: name_tok.clone(),
                        kind: ErrorKind::ExtendedError(format!("Expected a TypeBindingVar, got {:?}", v)),
                    })
                }
            },
            v => {
                return Err(Error {
                    span: name_tok.span.clone(),
                    tok: name_tok.clone(),
                    kind: ErrorKind::ExtendedError(format!("Expected a ExtendedTokenKind, got {:?}", v)),
                })
            }
        };
        parser::bump(ps, self);
        Ok(ExtTerm {
            span: name_tok.span.clone(),
            kind: ExtKind::Extended(StructDataKind::StructIdent(name_val)),
        })
    }

    fn parser_ty_bump_if(
        &mut self,
        ps: &mut ParserState<StructDataTokenKind, StructDataKind, StructDataPattern, StructDataType, StructDataState>,
    ) -> bool {
        match &ps.token.kind {
            ExtTokenKind::Extended(StructDataTokenKind::TypeBindingVar(_)) => true,
            _ => false,
        }
    }

    fn parser_ty(
        &mut self,
        ps: &mut ParserState<StructDataTokenKind, StructDataKind, StructDataPattern, StructDataType, StructDataState>,
    ) -> Result<Type<StructDataType>, Error<StructDataTokenKind>> {
        let binding = ps.token.kind.clone();

        let ExtTokenKind::Extended(StructDataTokenKind::TypeBindingVar(type_decl_key)) = binding else {
            return Err(Error {
                span: ps.span,
                tok: ps.token.clone(),
                kind: ErrorKind::ExtendedError(format!("Expected a TypeBindingVar, got {:?}", binding)),
            });
        };
        parser::bump(ps, self);

        let applied_types = if ps.token.kind == ExtTokenKind::LSquare {
            pulls_types_from_tyapp(ps, self)?
        } else {
            Vec::new()
        };

        let type_alias_app = Type::Extended(StructDataType::TypeAliasApp(type_decl_key, applied_types));
        Ok(type_alias_app)
    }
}

pub fn has_holed_type_named(
    ps: &mut ParserState<StructDataTokenKind, StructDataKind, StructDataPattern, StructDataType, StructDataState>,
    key: &str,
) -> Result<bool, Error<StructDataTokenKind>> {
    Ok(get_holed_type_from(ps, key).is_ok())
}

pub fn get_holed_type_from(
    ps: &mut ParserState<StructDataTokenKind, StructDataKind, StructDataPattern, StructDataType, StructDataState>,
    key: &str,
) -> Result<(usize, Type<StructDataType>), Error<StructDataTokenKind>> {
    match ps.ext_state.type_map.get(key) {
        Some(t) => Ok(t.clone()),
        None => Err(Error {
            span: ps.span,
            tok: ps.token.clone(),
            kind: ErrorKind::ExtendedError(format!("Tried to get holed type named '{:?}', found None", key)),
        }),
    }
}

pub fn set_holed_type_for(
    ps: &mut ParserState<StructDataTokenKind, StructDataKind, StructDataPattern, StructDataType, StructDataState>,
    key: &str,
    ty: (usize, Type<StructDataType>),
) -> Result<(), Error<StructDataTokenKind>> {
    match ps.ext_state.type_map.contains_key(key) {
        false => {
            ps.ext_state.type_map.insert(key.to_owned(), ty);
            Ok(())
        }
        true => Err(Error {
            span: ps.span,
            tok: ps.token.clone(),
            kind: ErrorKind::ExtendedError(format!("key {:?} is already present in types hash", key)),
        }),
    }
}

pub fn reify_type<'s>(
    ps: &mut ParserState<StructDataTokenKind, StructDataKind, StructDataPattern, StructDataType, StructDataState>,
    ext: &mut StructDataExtension,
    type_decl_key: &str,
    applied_types: &Vec<Type<StructDataType>>,
) -> Result<Type<StructDataType>, Error<StructDataTokenKind>> {
    let (tyabs_count, mut holed_type) = get_holed_type_from(ps, type_decl_key)?;

    if applied_types.len() != tyabs_count {
        return Err(Error {
            span: ps.span,
            tok: ps.token.clone(),
            kind: ErrorKind::ExtendedError(format!(
                "Expected tyabs count in holed type of be {:?}, but was {:?}",
                applied_types.len(),
                tyabs_count
            )),
        });
    }

    // FIXME need to know if we do this in forward or
    // reverse order!
    for ty in applied_types {
        // FIXME error if there's no substitution
        Subst::new(ty.clone()).visit(&mut holed_type)
    }

    // FIXME need to confirm it is TmVar free at this point, falls out of
    // check above

    Ok(holed_type)
}

pub fn pulls_types_from_tyapp(
    ps: &mut ParserState<StructDataTokenKind, StructDataKind, StructDataPattern, StructDataType, StructDataState>,
    ext: &mut StructDataExtension,
) -> Result<Vec<Type<StructDataType>>, Error<StructDataTokenKind>> {
    parser::expect(ps, ext, ExtTokenKind::LSquare)?;
    parser::expect(ps, ext, ExtTokenKind::Of)?;

    // FIXME: could be 1 or more
    let mut ret_val = Vec::new();
    loop {
        let ty = parser::ty(ps, ext)?;
        ret_val.push(ty);

        if ps.token.kind == ExtTokenKind::RSquare {
            break;
        }
        if ps.token.kind == ExtTokenKind::Comma {
            continue;
        }
        return Err(Error {
            span: ps.span,
            tok: ps.token.clone(),
            kind: ErrorKind::ExtendedError("Expected either end of tyapp or comma".to_owned()),
        });
    }

    parser::expect(ps, ext, ExtTokenKind::RSquare)?;
    Ok(ret_val)
}

pub fn extract_tyabs_for_type_shape<'s>(
    ps: &mut ParserState<'s, StructDataTokenKind, StructDataKind, StructDataPattern, StructDataType, StructDataState>,
    ext: &mut StructDataExtension,
) -> Result<usize, Error<StructDataTokenKind>> {
    parser::expect(ps, ext, ExtTokenKind::Lambda)?;
    let tyvar = parser::uppercase_id(ps, ext)?;
    let index = ps.tyvar.push(tyvar);
    let tyabs: Box<Type<StructDataType>> = Box::new(Type::Var(index));
    Ok(1) // FIXME do dynamic tyabs extraction and return the count
}

pub fn get_holed_type_from_decl<'s>(
    ps: &mut ParserState<'s, StructDataTokenKind, StructDataKind, StructDataPattern, StructDataType, StructDataState>,
    ext: &mut StructDataExtension,
) -> Result<(usize, Type<StructDataType>), Error<StructDataTokenKind>> {
    let sp = ps.span;
    let push_count = extract_tyabs_for_type_shape(ps, ext)?;

    // type def
    let type_shape = parser::once(
        ps,
        |p| parser::ty_atom(p, &mut StructDataExtension),
        "abstraction body required",
    )?;

    // pop off the tyabs var, since they only apply for the purpose of defining
    // "type holes" in the type def
    for _ in 0..push_count {
        ps.tyvar.pop();
    }

    Ok((push_count, type_shape))
}

impl SystemRTranslator<StructDataPattern, StructDataKind, StructDataType, StructDataState, BottomPattern, BottomKind, BottomType>
    for StructDataExtension
{
    fn resolve(
        &self,
        st: &mut StructDataState,
        tm: ExtTerm<StructDataPattern, StructDataKind, StructDataType>,
    ) -> Result<ExtTerm<BottomPattern, BottomKind, BottomType>, Diagnostic> {
        Err(Diagnostic::error(tm.span, format!("BottomExtension::resolve() unimpl")))
    }
}
