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

use crate::bottom::BottomPattern;
use crate::extensions::SystemRExtension;
use crate::system_r_util::span::Span;
use crate::terms::{ExtKind, ExtTerm, Literal};
use crate::types::{variant_field, Type};
use crate::visit::PatternVisitor;

pub type Pattern = ExtPattern<BottomPattern>;

/// Patterns for case and let expressions
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Hash)]
pub enum ExtPattern<TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default> {
    /// Wildcard pattern, this always matches
    Any,
    /// Constant pattern
    Literal(Literal),
    /// Variable binding pattern, this always matches
    Variable(String),
    /// Tuple of pattern bindings
    Product(Vec<ExtPattern<TExtPat>>),
    /// Algebraic datatype constructor, along with binding pattern
    Constructor(String, Box<ExtPattern<TExtPat>>),
    Extended(TExtPat),
}

#[derive(Clone, Debug, Default)]
pub struct PatVarStack<
    TExtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
> {
    pub inner: Vec<String>,
    _pat: TExtPat,
    _kind: TExtKind,
}

impl<
        TExtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
        TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    > PatVarStack<TExtPat, TExtKind>
{
    pub fn collect(pat: &ExtPattern<TExtPat>) -> Vec<String> {
        let mut p = Self::default();
        p.visit_pattern(pat);
        p.inner
    }
}

impl<
        TExtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
        TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    > PatternVisitor<TExtPat, TExtKind> for PatVarStack<TExtPat, TExtKind>
{
    fn visit_variable(&mut self, var: &str) {
        self.inner.push(var.to_owned());
    }
}

/// Visitor that simply counts the number of binders (variables) within a
/// pattern
pub struct PatternCount<
    TExtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
>(pub usize, pub TExtPat, pub TExtKind);

impl<
        TExtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
        TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    > PatternCount<TExtPat, TExtKind>
{
    pub fn collect(pat: &ExtPattern<TExtPat>) -> usize {
        let mut p = PatternCount(0, TExtPat::default(), TExtKind::default());
        p.visit_pattern(pat);
        p.0
    }
}

impl<
        TExtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
        TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    > PatternVisitor<TExtPat, TExtKind> for PatternCount<TExtPat, TExtKind>
{
    fn visit_variable(&mut self, var: &str) {
        self.0 += 1;
    }
}

impl<TExtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd> ExtPattern<TExtPat> {
    /// Does this pattern match the given [`Term`]?
    pub fn matches<
        TExtTokenKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
        TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
        TExtState: fmt::Debug + Default + Clone,
        TPtE: SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtState>,
    >(
        &self,
        term: &ExtTerm<TExtPat, TExtKind>,
        ext: &TPtE,
    ) -> bool {
        match self {
            ExtPattern::Any => return true,
            ExtPattern::Variable(_) => return true,
            ExtPattern::Literal(l) => {
                if let ExtKind::Lit(l2) = &term.kind {
                    return l == l2;
                }
            }
            ExtPattern::Product(vec) => {
                if let ExtKind::Product(terms) = &term.kind {
                    return vec.iter().zip(terms).all(|(p, t)| p.matches(t, ext));
                }
            }
            ExtPattern::Constructor(label, inner) => {
                if let ExtKind::Injection(label_, tm, _) = &term.kind {
                    if label == label_ {
                        return inner.matches(tm, ext);
                    }
                }
            }
            ExtPattern::Extended(v) => return ext.pat_ext_matches(v, term),
        }
        false
    }
}

/// Helper struct to traverse a [`Pattern`] and bind variables
/// to the typing context as needed.
///
/// It is the caller's responsibiliy to track stack growth and pop off
/// types after calling this function
pub struct PatTyStack<
    'ty,
    TExtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
> {
    pub ty: &'ty Type,
    pub inner: Vec<&'ty Type>,
    _pat: TExtPat,
    _kind: TExtKind,
}

impl<
        'ty,
        TExtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
        TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    > PatTyStack<'ty, TExtPat, TExtKind>
{
    pub fn collect(ty: &'ty Type, pat: &ExtPattern<TExtPat>) -> Vec<&'ty Type> {
        let mut p = PatTyStack {
            ty,
            inner: Vec::with_capacity(16),
            _pat: TExtPat::default(),
            _kind: TExtKind::default(),
        };
        p.visit_pattern(pat);
        p.inner
    }
}

impl<
        TExtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
        TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    > PatternVisitor<TExtPat, TExtKind> for PatTyStack<'_, TExtPat, TExtKind>
{
    fn visit_product(&mut self, pats: &Vec<ExtPattern<TExtPat>>) {
        if let Type::Product(tys) = self.ty {
            let ty = self.ty;
            for (ty, pat) in tys.iter().zip(pats.iter()) {
                self.ty = ty;
                self.visit_pattern(pat);
            }
            self.ty = ty;
        }
    }

    fn visit_constructor(&mut self, label: &str, pat: &ExtPattern<TExtPat>) {
        if let Type::Variant(vs) = self.ty {
            let ty = self.ty;
            self.ty = variant_field(vs, label, Span::zero()).unwrap();
            self.visit_pattern(pat);
            self.ty = ty;
        }
    }

    fn visit_ext(&mut self, p: &TExtPat) {}

    fn visit_pattern(&mut self, pattern: &ExtPattern<TExtPat>) {
        match pattern {
            ExtPattern::Any | ExtPattern::Literal(_) => {}
            ExtPattern::Variable(_) => self.inner.push(self.ty),
            ExtPattern::Constructor(label, pat) => self.visit_constructor(label, pat),
            ExtPattern::Product(pats) => self.visit_product(pats),
            ExtPattern::Extended(p) => self.visit_ext(p),
        }
    }
}

#[cfg(test)]
mod test {

    use crate::bottom::BottomKind;

    use super::*;
    #[test]
    fn pattern_count() {
        let mut pat: Pattern = ExtPattern::Variable(String::new());
        assert_eq!(PatternCount::<BottomPattern, BottomKind>::collect(&mut pat), 1);
    }

    #[test]
    fn pattern_ty_stack() {
        let mut pat: Pattern = ExtPattern::Variable(String::new());
        let ty = Type::Nat;
        assert_eq!(
            PatTyStack::<BottomPattern, BottomKind>::collect(&ty, &mut pat),
            vec![&ty]
        );
    }

    #[test]
    fn pattern_var_stack() {
        let mut pat: Pattern = ExtPattern::Variable("x".into());
        assert_eq!(
            PatVarStack::<BottomPattern, BottomKind>::collect(&mut pat),
            vec![String::from("x")]
        );
    }
}
