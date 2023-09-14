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
use core::fmt;
use std::{collections::HashMap};

use crate::system_r_util::span::Span;

use crate::{types::{ExtContext, Type}, diagnostics::Diagnostic, terms::{Term, Kind}, bottom::{BottomPattern, BottomKind, BottomExtension}, patterns::PatternExtension};

pub type WrappedFn = fn(input: Term, span: &Span) -> Result<Term, Diagnostic>;

#[derive(Debug, Default, Clone)]
pub struct PlatformBindingDecl {
    pub ty_abs: Vec<Kind>,
    pub args: Vec<Type>,
    pub ret_type: Type,
}

pub struct WrappedContent(pub WrappedFn, pub Type, pub Type);

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
    by_idx: Vec<WrappedContent>
}

impl PartialEq for PlatformBindings {
    fn eq(&self, other: &Self) -> bool {
        return self.by_name == other.by_name;
    }
}

pub trait UnwrappedFnTrait {
    fn to_wrapped(&self) -> WrappedFn;
}

impl<'a> PlatformBindings {
    pub fn new() -> Self {
        PlatformBindings { by_name: HashMap::new(), by_idx: Vec::new() }
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
        match self.by_name.insert(alias.to_owned(), idx) {
            Some(_) => Some(wrapped),
            None => None
        }
    }
    
    pub fn get(&'a self, idx: usize) -> Option<&'a WrappedContent> {
        self.by_idx.get(idx)
    }
}

impl<TExtPat: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
         TExtKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
         TPtE: Clone + Default + PatternExtension<TExtPat, TExtKind>> ExtContext<TExtPat, TExtKind, TPtE> {
    pub(crate) fn type_check_platform_binding(&mut self, idx: &usize, span: &Span) -> Result<Type, Diagnostic> {
        match self.platform_bindings.get(*idx) {
            Some(wc) => Ok(Type::PlatformBinding(Box::new(wc.1.clone()), Box::new(wc.2.clone()))),
            None => Err(Diagnostic::error(*span, format!("No matching platform_binding registration for idx {}", idx)))
        }
    }
}