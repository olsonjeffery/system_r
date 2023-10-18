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
//! Representation lambda calculus terms
use crate::bottom::{BottomKind, BottomPattern, BottomType, BottomDialect};
use crate::extensions::SystemRDialect;
use crate::patterns::Pattern;
use crate::system_r_util::span::Span;
use crate::types::Type;
use std::{fmt, hash};
pub mod visit;

pub type Term = ExtTerm<BottomDialect>;
#[derive(Clone, Default, PartialEq, PartialOrd)]
pub struct ExtTerm<
    TExtDialect: SystemRDialect + PartialEq + PartialOrd + Default + fmt::Debug + Clone,
> {
    pub span: Span,
    pub kind: ExtKind<TExtDialect>,
}

/// Primitive functions supported by this implementation
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum Primitive {
    Succ,
    Pred,
    IsZero,
}

/// Abstract syntax of the parametric polymorphic lambda calculus
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum ExtKind<
    TExtDialect: SystemRDialect + PartialEq + PartialOrd + Default + fmt::Debug + Clone,
> {
    /// A literal value
    Lit(Literal),
    /// A bound variable, represented by it's de Bruijn index
    Var(usize),
    /// A re
    PlatformBinding(usize),
    /// An intrinsic, referenced by alias
    /// Fixpoint operator/Y combinator
    Fix(Box<ExtTerm<TExtDialect>>),

    Primitive(Primitive),

    /// Injection into a sum type
    /// fields: type constructor tag, term, and sum type
    Injection(String, Box<ExtTerm<TExtDialect>>, Box<Type<TExtDialect::TExtType>>),

    /// Product type (tuple)
    Product(Vec<ExtTerm<TExtDialect>>),
    /// Projection into a term
    Projection(Box<ExtTerm<TExtDialect>>, usize),

    /// A case expr, with case arms
    Case(
        Box<ExtTerm<TExtDialect>>,
        Vec<Arm<TExtDialect>>,
    ),

    // let expr with binding, value and then applied context
    Let(
        Box<Pattern<TExtDialect>>,
        Box<ExtTerm<TExtDialect>>,
        Box<ExtTerm<TExtDialect>>,
    ),
    /// A lambda abstraction
    Abs(Box<Type<TExtDialect::TExtType>>, Box<ExtTerm<TExtDialect>>),
    /// Application of a term to another term
    App(
        Box<ExtTerm<TExtDialect>>,
        Box<ExtTerm<TExtDialect>>,
    ),
    /// Type abstraction
    TyAbs(Box<ExtTerm<TExtDialect>>),
    /// Type application
    TyApp(Box<ExtTerm<TExtDialect>>, Box<Type<TExtDialect::TExtType>>),

    Fold(Box<Type<TExtDialect::TExtType>>, Box<ExtTerm<TExtDialect>>),
    Unfold(Box<Type<TExtDialect::TExtType>>, Box<ExtTerm<TExtDialect>>),

    /// Introduce an existential type
    /// { *Ty1, Term } as {∃X.Ty}
    /// essentially, concrete representation as interface
    Pack(
        Box<Type<TExtDialect::TExtType>>,
        Box<ExtTerm<TExtDialect>>,
        Box<Type<TExtDialect::TExtType>>,
    ),
    /// Unpack an existential type
    /// open {∃X, bind} in body -- X is bound as a TyVar, and bind as Var(0)
    /// Eliminate an existential type
    Unpack(
        Box<ExtTerm<TExtDialect>>,
        Box<ExtTerm<TExtDialect>>,
    ),

    /// Extension
    Extended(TExtDialect::TExtKind),
}

impl<
        TExtDialect: SystemRDialect + PartialEq + PartialOrd + Default + fmt::Debug + Clone,
    > Default for ExtKind<TExtDialect>
{
    fn default() -> Self {
        ExtKind::Lit(Literal::Unit)
    }
}

/// Arm of a case expression
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Arm<
    TExtDialect: SystemRDialect + PartialEq + PartialOrd + Default + fmt::Debug + Clone,
> {
    pub span: Span,
    pub pat: Pattern<TExtDialect>,
    pub term: Box<ExtTerm<TExtDialect>>,
    pub _d: TExtDialect,
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
    TExtDialect: SystemRDialect + PartialEq + PartialOrd + Default + fmt::Debug + Clone,
    > ExtTerm<TExtDialect>
{
    pub fn new(kind: ExtKind<TExtDialect>, span: Span) -> ExtTerm<TExtDialect> {
        ExtTerm { span, kind }
    }

    #[allow(dead_code)]
    pub const fn unit() -> ExtTerm<TExtDialect> {
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
    pub fn kind(&self) -> &ExtKind<TExtDialect> {
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
    TExtDialect: SystemRDialect + PartialEq + PartialOrd + Default + fmt::Debug + Clone,
    > fmt::Display for ExtTerm<TExtDialect>
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
    TExtDialect: SystemRDialect + PartialEq + PartialOrd + Default + fmt::Debug + Clone,
    > fmt::Debug for ExtTerm<TExtDialect>
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

        let a_pats = vec![con!("A", Pattern::Any), con!("A", num!(9)), con!("A", num!(10))];

        let b_pats = vec![
            con!("B", Pattern::Any),
            con!("B", prod!(num!(1), boolean!(true))),
            con!("B", prod!(Pattern::Any, boolean!(false))),
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
