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
use core::fmt;
use std::collections::HashMap;
use std::hash;

use crate::bottom::{BottomDialect, BottomKind, BottomPattern, BottomTokenKind, BottomType};
use crate::extensions::{SystemRDialect, SystemRExtension};
use crate::system_r_util::span::Span;

use crate::terms::Term;
use crate::types::Variant;
use crate::{
    diagnostics::Diagnostic,
    types::{Context, Type},
};

pub type WrappedFn = fn(input: Term<BottomDialect>, span: &Span) -> Result<Term<BottomDialect>, Diagnostic>;

pub struct WrappedContent(pub WrappedFn, pub Type<BottomDialect>, pub Type<BottomDialect>);

impl Clone for WrappedContent {
    fn clone(&self) -> Self {
        WrappedContent(self.0, self.1.clone(), self.2.clone())
    }
}

impl core::fmt::Debug for WrappedContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Intrinsic WrappedContent").finish()
    }
}

#[derive(Default, Clone, Debug)]
pub struct PlatformBindings {
    by_name: HashMap<String, usize>,
    by_idx: Vec<WrappedContent>,
}

impl PartialEq for PlatformBindings {
    fn eq(&self, other: &Self) -> bool {
        self.by_name == other.by_name
    }
}

pub trait UnwrappedFnTrait {
    fn to_wrapped(&self) -> WrappedFn;
}

impl<'a> PlatformBindings {
    pub fn new() -> Self {
        PlatformBindings {
            by_name: HashMap::new(),
            by_idx: Vec::new(),
        }
    }

    /// takes a &str alias and returns Some(usize) if that alias is
    /// registered with this PlatformBindings instance
    pub fn has(&self, alias: &str) -> Option<usize> {
        let r = self.by_name.get(alias);
        let Some(idx) = r else {
            return None;
        };
        Some(*idx)
    }

    pub fn register(&mut self, alias: &str, wrapped: WrappedContent) -> Option<WrappedContent> {
        self.by_idx.push(wrapped.clone());
        let idx = self.by_idx.len() - 1;
        self.by_name.insert(alias.to_owned(), idx).map(|_| wrapped)
    }

    pub fn get(&'a self, idx: usize) -> Option<&'a WrappedContent> {
        self.by_idx.get(idx)
    }
}

fn resolve_pb_type<
    TExtDialect: Eq + SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash,
>(
    ty_in: Type<BottomDialect>,
) -> Result<Type<TExtDialect>, Diagnostic> {
    match ty_in {
        Type::Extended(v) => Err(Diagnostic::error(
            Default::default(),
            "Platform binding with extended args type; not allowed",
        )),
        Type::Alias(v) => Ok(Type::Alias(v)),
        Type::Arrow(a, b) => Ok(Type::Arrow(
            Box::new(resolve_pb_type(*a)?),
            Box::new(resolve_pb_type(*b)?),
        )),
        Type::Bool => Ok(Type::Bool),
        Type::Existential(a) => Ok(Type::Existential(Box::new(resolve_pb_type(*a)?))),
        Type::Nat => Ok(Type::Nat),
        Type::PlatformBinding(_, _) => Err(Diagnostic::error(
            Default::default(),
            "Platforming with platform-binding-type vars; shouldn't happen",
        )),
        Type::Product(v) => {
            let mut result = Vec::new();
            for i in v {
                result.push(resolve_pb_type(i)?);
            }
            return Ok(Type::Product(result));
        }
        Type::Rec(r) => Ok(Type::Rec(Box::new(resolve_pb_type(*r)?))),
        Type::Tag(t) => Ok(Type::Tag(t)),
        Type::Unit => Ok(Type::Unit),
        Type::Var(s) => Ok(Type::Var(s)),
        Type::Variant(v) => {
            let mut result = Vec::new();
            for i in v {
                let variant: Variant<TExtDialect> = Variant {
                    label: i.label,
                    ty: resolve_pb_type(i.ty)?,
                };
                result.push(variant);
            }
            return Ok(Type::Variant(result));
        }
        Type::Universal(r) => Ok(Type::Universal(Box::new(resolve_pb_type(*r)?))),
    }
}

impl<TExtDialect: Eq + hash::Hash + PartialEq + PartialOrd + SystemRDialect + Clone + fmt::Debug + Default>
    Context<TExtDialect>
{
    pub(crate) fn type_check_platform_binding(
        &mut self,
        idx: &usize,
        span: &Span,
    ) -> Result<Type<TExtDialect>, Diagnostic> {
        match self.platform_bindings.get(*idx) {
            Some(wc) => {
                let args_resolved = resolve_pb_type(wc.1.clone())?;
                let ret_resolved = resolve_pb_type(wc.2.clone())?;
                return Ok(Type::PlatformBinding(Box::new(args_resolved), Box::new(ret_resolved)));
            }
            None => Err(Diagnostic::error(
                *span,
                format!("No matching platform_binding registration for idx {}", idx),
            )),
        }
    }
}
