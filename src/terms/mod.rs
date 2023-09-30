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
//! Representation lambda calculus terms
use crate::bottom::{BottomKind, BottomPattern};
use crate::patterns::ExtPattern;
use crate::system_r_util::span::Span;
use crate::types::Type;
use std::fmt;
pub mod visit;

pub type Term = ExtTerm<BottomPattern, BottomKind>;

#[derive(Clone, Default, PartialEq, PartialOrd)]
pub struct ExtTerm<
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
> {
    pub span: Span,
    pub kind: ExtKind<TExtPat, TExtKind>,
}

/// Primitive functions supported by this implementation
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Primitive {
    Succ,
    Pred,
    IsZero,
}

pub type Kind = ExtKind<BottomPattern, BottomKind>;

/// Abstract syntax of the parametric polymorphic lambda calculus
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum ExtKind<
    TExtPat: Default + Clone + fmt::Debug + PartialEq + PartialOrd,
    TExtKind: Default + Clone + fmt::Debug + PartialEq + PartialOrd,
> {
    /// A literal value
    Lit(Literal),
    /// A bound variable, represented by it's de Bruijn index
    Var(usize),
    /// A re
    PlatformBinding(usize),
    /// An intrinsic, referenced by alias
    /// Fixpoint operator/Y combinator
    Fix(Box<ExtTerm<TExtPat, TExtKind>>),

    Primitive(Primitive),

    /// Injection into a sum type
    /// fields: type constructor tag, term, and sum type
    Injection(String, Box<ExtTerm<TExtPat, TExtKind>>, Box<Type>),

    /// Product type (tuple)
    Product(Vec<ExtTerm<TExtPat, TExtKind>>),
    /// Projection into a term
    Projection(Box<ExtTerm<TExtPat, TExtKind>>, usize),

    /// A case expr, with case arms
    Case(Box<ExtTerm<TExtPat, TExtKind>>, Vec<Arm<TExtPat, TExtKind>>),

    // let expr with binding, value and then applied context
    Let(
        Box<ExtPattern<TExtPat>>,
        Box<ExtTerm<TExtPat, TExtKind>>,
        Box<ExtTerm<TExtPat, TExtKind>>,
    ),
    /// A lambda abstraction
    Abs(Box<Type>, Box<ExtTerm<TExtPat, TExtKind>>),
    /// Application of a term to another term
    App(Box<ExtTerm<TExtPat, TExtKind>>, Box<ExtTerm<TExtPat, TExtKind>>),
    /// Type abstraction
    TyAbs(Box<ExtTerm<TExtPat, TExtKind>>),
    /// Type application
    TyApp(Box<ExtTerm<TExtPat, TExtKind>>, Box<Type>),

    Fold(Box<Type>, Box<ExtTerm<TExtPat, TExtKind>>),
    Unfold(Box<Type>, Box<ExtTerm<TExtPat, TExtKind>>),

    /// Introduce an existential type
    /// { *Ty1, Term } as {∃X.Ty}
    /// essentially, concrete representation as interface
    Pack(Box<Type>, Box<ExtTerm<TExtPat, TExtKind>>, Box<Type>),
    /// Unpack an existential type
    /// open {∃X, bind} in body -- X is bound as a TyVar, and bind as Var(0)
    /// Eliminate an existential type
    Unpack(Box<ExtTerm<TExtPat, TExtKind>>, Box<ExtTerm<TExtPat, TExtKind>>),
    /// Extension
    Extended(TExtKind),
}

impl<
        TExtPat: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
        TExtKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    > Default for ExtKind<TExtPat, TExtKind>
{
    fn default() -> Self {
        ExtKind::Lit(Literal::Unit)
    }
}

/// Arm of a case expression
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Arm<
    TExtPat: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    TExtKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
> {
    pub span: Span,
    pub pat: ExtPattern<TExtPat>,
    pub term: Box<ExtTerm<TExtPat, TExtKind>>,
    pub _kind: TExtKind,
}

/// Constant literal expression or pattern
#[derive(Clone, Default, Debug, PartialEq, PartialOrd, Eq, Hash)]
pub enum Literal {
    #[default]
    Unit,
    Bool(bool),
    Nat(u32),
    Tag(String),
}

impl<
        TExtPat: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
        TExtKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    > ExtTerm<TExtPat, TExtKind>
{
    pub fn new(kind: ExtKind<TExtPat, TExtKind>, span: Span) -> ExtTerm<TExtPat, TExtKind> {
        ExtTerm { span, kind }
    }

    #[allow(dead_code)]
    pub const fn unit() -> ExtTerm<TExtPat, TExtKind> {
        ExtTerm {
            span: Span::dummy(),
            kind: ExtKind::Lit(Literal::Unit),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }

    #[inline]
    pub fn kind(&self) -> &ExtKind<TExtPat, TExtKind> {
        &self.kind
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Nat(n) => write!(f, "{}", n),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Unit => write!(f, "unit"),
            Literal::Tag(s) => write!(f, "{}", s),
        }
    }
}

impl<
        TExtPat: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
        TExtKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    > fmt::Display for ExtTerm<TExtPat, TExtKind>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ExtKind::Lit(lit) => write!(f, "{}", lit),
            ExtKind::PlatformBinding(idx) => write!(f, "PlatformBinding({})", idx),
            ExtKind::Var(v) => write!(f, "#{}", v),
            ExtKind::Abs(ty, term) => write!(f, "(λ_:{:?}. {})", ty, term),
            ExtKind::Fix(term) => write!(f, "Fix {:?}", term),
            ExtKind::Primitive(p) => write!(f, "{:?}", p),
            ExtKind::Injection(label, tm, ty) => write!(f, "{}({})", label, tm),
            ExtKind::Projection(term, idx) => write!(f, "{}.{}", term, idx),
            ExtKind::Product(terms) => write!(
                f,
                "({})",
                terms
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            ExtKind::Case(term, arms) => {
                writeln!(f, "case {} of", term)?;
                for arm in arms {
                    writeln!(f, "\t| {:?} => {},", arm.pat, arm.term)?;
                }
                write!(f, "")
            }
            ExtKind::Let(pat, t1, t2) => write!(f, "let {:?} = {} in {}", pat, t1, t2),
            ExtKind::App(t1, t2) => write!(f, "({} {})", t1, t2),
            ExtKind::TyAbs(term) => write!(f, "(λTy {})", term),
            ExtKind::TyApp(term, ty) => write!(f, "({} [{:?}])", term, ty),
            ExtKind::Fold(ty, term) => write!(f, "fold [{:?}] {}", ty, term),
            ExtKind::Unfold(ty, term) => write!(f, "unfold [{:?}] {}", ty, term),
            ExtKind::Pack(witness, body, sig) => write!(f, "[|pack {{*{:?}, {}}} as {:?} |]", witness, body, sig),
            ExtKind::Unpack(m, n) => write!(f, "unpack {} as {}", m, n),
            ExtKind::Extended(k) => write!(f, "extended (kind: {:?})", k),
        }
    }
}

impl<
        TExtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
        TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    > fmt::Debug for ExtTerm<TExtPat, TExtKind>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

#[cfg(test)]
mod test {
    use crate::bottom::BottomExtension;

    use super::*;

    #[test]
    fn pattern_matches() {
        let ty = Type::Variant(vec![
            variant!("A", Type::Nat),
            variant!("B", Type::Product(vec![Type::Nat, Type::Bool])),
        ]);

        let a_pats = vec![con!("A", ExtPattern::Any), con!("A", num!(9)), con!("A", num!(10))];

        let b_pats = vec![
            con!("B", ExtPattern::Any),
            con!("B", prod!(num!(1), boolean!(true))),
            con!("B", prod!(ExtPattern::Any, boolean!(false))),
        ];

        let res = [true, false, true];

        let a = inj!("A", nat!(10), ty.clone());
        let b = inj!("B", tuple!(nat!(1), lit!(false)), ty.clone());

        for (pat, result) in a_pats.iter().zip(&res) {
            assert_eq!(pat.matches(&a, &BottomExtension), *result);
        }

        for (pat, result) in b_pats.iter().zip(&res) {
            assert_eq!(pat.matches(&b, &BottomExtension), *result, "{:?}", pat);
        }
    }
}
