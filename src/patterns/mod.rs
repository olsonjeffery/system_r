/*
Copyright (C) 2020-2023 Micheal Lazear, AUTHORS

The MIT License (MIT)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

---------------------

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
use std::hash;

use crate::extensions::{SystemRDialect, SystemRExtension};
use crate::system_r_util::span::Span;
use crate::terms::{Kind, Literal, Term};
use crate::types::{variant_field, Type};
use crate::visit::PatternVisitor;

/// Patterns for case and let expressions
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Hash)]
pub enum Pattern<TExtDialect: Eq + SystemRDialect + Clone + fmt::Debug + Default> {
    /// Wildcard pattern, this always matches
    Any,
    /// Constant pattern
    Literal(Literal),
    /// Variable binding pattern, this always matches
    Variable(String),
    /// Tuple of pattern bindings
    Product(Vec<Pattern<TExtDialect>>),
    /// Algebraic datatype constructor, along with binding pattern
    Constructor(String, Box<Pattern<TExtDialect>>),
    Extended(TExtDialect::TExtPat),
}

#[derive(Clone, Debug, Default)]
pub struct PatVarStack<TExtDialect: Eq + SystemRDialect + Default + fmt::Debug + Clone + PartialEq + PartialOrd> {
    pub inner: Vec<String>,
    _d: TExtDialect,
}

impl<TExtDialect: hash::Hash + Eq + SystemRDialect + Default + fmt::Debug + Clone + PartialEq + PartialOrd>
    PatVarStack<TExtDialect>
{
    pub fn collect(pat: &Pattern<TExtDialect>) -> Vec<String> {
        let mut p = Self::default();
        p.visit_pattern(pat);
        p.inner
    }
}

impl<TExtDialect: hash::Hash + Eq + SystemRDialect + Default + fmt::Debug + Clone + PartialEq + PartialOrd>
    PatternVisitor<TExtDialect> for PatVarStack<TExtDialect>
{
    fn visit_variable(&mut self, var: &str) {
        self.inner.push(var.to_owned());
    }
}

/// Visitor that simply counts the number of binders (variables) within a
/// pattern
pub struct PatternCount<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd,
>(pub usize, pub TExtDialect::TExtPat, pub TExtDialect::TExtKind);

impl<TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd>
    PatternCount<TExtDialect>
{
    pub fn collect(pat: &Pattern<TExtDialect>) -> usize {
        let mut p = PatternCount(0, Default::default(), Default::default());
        p.visit_pattern(pat);
        p.0
    }
}

impl<TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd>
    PatternVisitor<TExtDialect> for PatternCount<TExtDialect>
{
    fn visit_variable(&mut self, var: &str) {
        self.0 += 1;
    }
}

impl<TExtDialect: hash::Hash + Eq + SystemRDialect + Default + fmt::Debug + Clone + PartialEq + PartialOrd>
    Pattern<TExtDialect>
{
    /// Does this pattern match the given [`Term`]?
    pub fn matches<TPtE: SystemRExtension<TExtDialect>>(&self, term: &Term<TExtDialect>, ext: &TPtE) -> bool {
        match self {
            Pattern::Any => return true,
            Pattern::Variable(_) => return true,
            Pattern::Literal(l) => {
                if let Kind::Lit(l2) = &term.kind {
                    return l == l2;
                }
            }
            Pattern::Product(vec) => {
                if let Kind::Product(terms) = &term.kind {
                    return vec.iter().zip(terms).all(|(p, t)| p.matches(t, ext));
                }
            }
            Pattern::Constructor(label, inner) => {
                if let Kind::Injection(label_, tm, _) = &term.kind {
                    if label == label_ {
                        return inner.matches(tm, ext);
                    }
                }
            }
            Pattern::Extended(v) => return ext.pat_ext_matches(v, term),
        }
        false
    }
}

/// Helper struct to traverse a [`Pattern`] and bind variables
/// to the typing context as needed.
///
/// It is the caller's responsibiliy to track stack growth and pop off
/// types after calling this function
pub struct PatTyStack<'ty, TExtDialect: Eq + SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd> {
    pub ty: &'ty Type<TExtDialect>,
    pub inner: Vec<&'ty Type<TExtDialect>>,
}

impl<'ty, TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd>
    PatTyStack<'ty, TExtDialect>
{
    pub fn collect(ty: &'ty Type<TExtDialect>, pat: &Pattern<TExtDialect>) -> Vec<&'ty Type<TExtDialect>> {
        let mut p = PatTyStack {
            ty,
            inner: Vec::with_capacity(16),
        };
        p.visit_pattern(pat);
        p.inner
    }
}

impl<TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd>
    PatternVisitor<TExtDialect> for PatTyStack<'_, TExtDialect>
{
    fn visit_product(&mut self, pats: &Vec<Pattern<TExtDialect>>) {
        if let Type::Product(tys) = self.ty {
            let ty = self.ty;
            for (ty, pat) in tys.iter().zip(pats.iter()) {
                self.ty = ty;
                self.visit_pattern(pat);
            }
            self.ty = ty;
        }
    }

    fn visit_constructor(&mut self, label: &str, pat: &Pattern<TExtDialect>) {
        if let Type::Variant(vs) = self.ty {
            let ty = self.ty;
            self.ty = variant_field::<TExtDialect>(vs, label, Span::zero()).unwrap();
            self.visit_pattern(pat);
            self.ty = ty;
        }
    }

    fn visit_ext(&mut self, p: &TExtDialect::TExtPat) {}

    fn visit_pattern(&mut self, pattern: &Pattern<TExtDialect>) {
        match pattern {
            Pattern::Any | Pattern::Literal(_) => {}
            Pattern::Variable(_) => self.inner.push(self.ty),
            Pattern::Constructor(label, pat) => self.visit_constructor(label, pat),
            Pattern::Product(pats) => self.visit_product(pats),
            Pattern::Extended(p) => self.visit_ext(p),
        }
    }
}

#[cfg(test)]
mod test {

    use crate::bottom::BottomDialect;

    use super::*;
    #[test]
    fn pattern_count() {
        let mut pat: Pattern<BottomDialect> = Pattern::Variable(String::new());
        assert_eq!(PatternCount::<BottomDialect>::collect(&mut pat), 1);
    }

    #[test]
    fn pattern_ty_stack() {
        let mut pat: Pattern<BottomDialect> = Pattern::Variable(String::new());
        let ty = Type::Nat;
        assert_eq!(PatTyStack::<BottomDialect>::collect(&ty, &mut pat), vec![&ty]);
    }

    #[test]
    fn pattern_var_stack() {
        let mut pat: Pattern<BottomDialect> = Pattern::Variable("x".into());
        assert_eq!(PatVarStack::<BottomDialect>::collect(&mut pat), vec![String::from("x")]);
    }
}
