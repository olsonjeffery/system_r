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
//! Naive, inefficient exhaustiveness checking for pattern matching
//!
//! Inspired somewhat by the docs for the Rust compiler (and linked paper), we
//! create a "usefulness" predicate. We store current patterns in a row-wise
//! [`Matrix`], and iterate through each row in the matrix every time we want
//! to add a new pattern. If no existing rows completely overlap the new row,
//! then we can determine that the new row is "useful", and add it.
//!
//! To check for exhaustiveness, we simply create a row of Wildcard matches,
//! and see if it would be useful to add
//!
//! https://doc.rust-lang.org/nightly/nightly-rustc/src/rustc_mir/hair/pattern/_match.rs.html
//! http://moscova.inria.fr/~maranget/papers/warn/index.html

use super::*;
use crate::diagnostics::*;
use crate::extensions::SystemRExtension;
use crate::patterns::{ExtPattern, PatTyStack};
use crate::terms::*;
use std::collections::HashSet;

/// Return true if `existing` covers `new`, i.e. if new is a useful pattern
/// then `overlap` will return `false`
fn overlap<
    TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
    TExtKind: Clone + fmt::Debug + PartialEq + PartialOrd + Default,
>(
    existing: &ExtPattern<TExtPat>,
    new: &ExtPattern<TExtPat>,
) -> bool {
    use ExtPattern::*;
    match (existing, new) {
        (Any, _) => true,
        (Variable(_), _) => true,
        (Constructor(l, a), Constructor(l2, b)) => {
            if l == l2 {
                overlap::<TExtPat, TExtKind>(a, b)
            } else {
                false
            }
        }
        (Product(a), Product(b)) => a.iter().zip(b.iter()).all(|(a, b)| overlap::<TExtPat, TExtKind>(a, b)),
        (Product(a), b) => a.iter().all(|a| overlap::<TExtPat, TExtKind>(a, b)),
        (x, y) => x == y,
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Matrix<'pat, TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default> {
    pub expr_ty: Type,
    len: usize,
    matrix: Vec<Vec<&'pat ExtPattern<TExtPat>>>,
}

impl<'pat, TExtPat: Clone + fmt::Debug + PartialEq + PartialOrd + Default> Matrix<'pat, TExtPat> {
    /// Create a new [`Matrix`] for a given type
    pub fn new(expr_ty: Type) -> Matrix<'pat, TExtPat> {
        let len = match &expr_ty {
            Type::Product(p) => p.len(),
            _ => 1,
        };

        Matrix {
            expr_ty,
            len,
            matrix: Vec::new(),
        }
    }

    /// Is the pattern [`Matrix`] exhaustive for this type?
    ///
    /// For a boolean type, True, False, or a wildcard/variable match are
    /// required
    ///
    /// For a product type (tuple), the algorithm is slightly more
    /// complicated: We generate a tuple of length N (equal to the length of
    /// the case expressions' tuple) filled with Wildcard patterns, and see
    /// if addition of tuple is a useful pattern. If the pattern is not
    /// useful (i.e. it totally `overlap's` with an existing row), then the
    /// matrix is exhaustive
    ///
    /// For a sum type, a dummy constructor of pattern `Const_i _` is generated
    /// for all `i` of the possible constructors of the type. If none of the
    /// dummy constructors are useful, then the current patterns are exhaustive
    pub fn exhaustive<TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd>(&self) -> bool {
        match &self.expr_ty {
            Type::Variant(v) => v.iter().all(|variant| {
                // For all constructors in the sum type, generate a constructor
                // pattern that will match all possible inhabitants of that
                // constructor
                let con = ExtPattern::Constructor(variant.label.clone(), Box::new(ExtPattern::Any));
                let temp = [&con];
                let mut ret = false;
                for row in &self.matrix {
                    if row.iter().zip(&temp).all(|(a, b)| overlap::<TExtPat, TExtKind>(a, b)) {
                        ret = true;
                        break;
                    }
                }
                ret
            }),
            Type::Product(_) | Type::Nat => {
                // Generate a tuple of wildcard patterns. If the pattern is
                // useful, then we do not have an exhaustive matrix
                let filler = (0..self.len).map(|_| ExtPattern::Any).collect::<Vec<_>>();
                for row in &self.matrix {
                    if row
                        .iter()
                        .zip(filler.iter())
                        .all(|(a, b)| overlap::<TExtPat, TExtKind>(a, b))
                    {
                        return true;
                    }
                }
                false
            }
            Type::Bool => {
                // Boolean type is one of the simplest cases: we only need
                // to match `true` and `false`, or one of those + a wildcard,
                // or just a wildcard
                let tru = ExtPattern::Literal(Literal::Bool(true));
                let fal = ExtPattern::Literal(Literal::Bool(false));
                !(self.can_add_row::<TExtKind>(vec![&tru]) && self.can_add_row::<TExtKind>(vec![&fal]))
            }
            Type::Unit => {
                // Unit is a degenerate case
                let unit = ExtPattern::Literal(Literal::Unit);
                !self.can_add_row::<TExtKind>(vec![&unit])
            }
            _ => false,
        }
    }

    /// Return true if a new pattern row is reachable
    fn can_add_row<TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd>(
        &self,
        new_row: Vec<&'pat ExtPattern<TExtPat>>,
    ) -> bool {
        assert_eq!(self.len, new_row.len());
        for row in &self.matrix {
            if row
                .iter()
                .zip(new_row.iter())
                .all(|(a, b)| overlap::<TExtPat, TExtKind>(a, b))
            {
                return false;
            }
        }
        true
    }

    fn try_add_row<TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd>(
        &mut self,
        new_row: Vec<&'pat ExtPattern<TExtPat>>,
    ) -> bool {
        assert_eq!(self.len, new_row.len());
        for row in &self.matrix {
            if row
                .iter()
                .zip(new_row.iter())
                .all(|(a, b)| overlap::<TExtPat, TExtKind>(a, b))
            {
                return false;
            }
        }
        self.matrix.push(new_row);
        true
    }

    /// Attempt to add a new [`Pattern`] to the [`Matrix`]
    ///
    /// Returns true on success, and false if the new pattern is
    /// unreachable
    pub fn add_pattern<
        TExtTokenKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
        TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
        TExtState: fmt::Debug + Default + Clone,
        TExt: Clone + fmt::Debug + Default + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtState>,
    >(
        &mut self,
        pat: &'pat ExtPattern<TExtPat>,
        ext: &mut TExt,
    ) -> bool {
        match pat {
            ExtPattern::Any | ExtPattern::Variable(_) => {
                let filler = (0..self.len).map(|_| &ExtPattern::Any).collect::<Vec<_>>();
                self.try_add_row::<TExtKind>(filler)
            }
            ExtPattern::Product(tuple) => self.try_add_row::<TExtKind>(tuple.iter().collect()),
            ExtPattern::Literal(lit) => {
                if self.len == 1 {
                    self.try_add_row::<TExtKind>(vec![pat])
                } else {
                    false
                }
            }
            ExtPattern::Constructor(label, inner) => self.try_add_row::<TExtKind>(vec![pat]),
            v @ ExtPattern::Extended(_) => ext.pat_add_ext_pattern(&(*self), v),
        }
    }
}

impl<
        TExtTokenKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
        TExtKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
        TExtPat: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    > ExtContext<TExtTokenKind, TExtKind, TExtPat>
{
    /// Type check a case expression, returning the Type of the arms, assuming
    /// that the case expression is exhaustive and well-typed
    ///
    /// This is one of the more complicated functions in the typechecker.
    /// 1) We first have to check that each arm in the case expression has
    /// a pattern that is proper for type of the case expression - we shouldn't
    /// have case arms with patterns that can be never be matched!
    ///
    /// 2) We then need to bind any variables referenced in the pattern into
    /// the typing context - `Cons (x, xs)` needs to bind both x and xs, as does
    /// just `Cons x`.
    ///
    /// 3) After variable binding, we need to typecheck the actual case arm's
    /// term, and store the result so that we can compare it to the types of
    /// the other arms in the case expression
    ///
    /// 4) If the arm is properly typed, then we need to add it to a matrix so
    /// that we can determine if the pattern is reachable, and if the case arms
    /// are exhaustive - one, and only one, pattern should be matchable
    ///
    /// 5) Finally, assuming all of the previous checks have passed, we return
    /// the shared type of all of the case arms - the term associated with each
    /// arm should have one type, and that type should be the same for all of
    /// the arms.
    pub(crate) fn type_check_case<
    TExtState: Clone + fmt::Debug + Default,
    TExt: Clone + fmt::Debug + Default + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtState>,
    >(
        &mut self,
        expr: &ExtTerm<TExtPat, TExtKind>,
        arms: &[Arm<TExtPat, TExtKind>],
        ext: &mut TExt,
    ) -> Result<Type, Diagnostic> {
        let ty = self.type_check(expr)?;
        let mut matrix = patterns::Matrix::<TExtPat>::new(ty);

        let mut set = HashSet::new();
        for arm in arms {
            if self.pattern_type_eq(&arm.pat, &matrix.expr_ty, ext) {
                let height = self.stack.len();

                let binds = PatTyStack::<TExtPat, TExtKind>::collect(&matrix.expr_ty, &arm.pat);
                for b in binds.into_iter().rev() {
                    self.push(b.clone());
                }

                let arm_ty = self.type_check(&arm.term)?;

                while self.stack.len() > height {
                    self.pop();
                }

                set.insert(arm_ty);
                if !matrix.add_pattern(&arm.pat, ext) {
                    return Err(Diagnostic::error(arm.span, "unreachable pattern!"));
                }
            } else {
                return Err(
                    Diagnostic::error(expr.span, format!("case binding has a type {:?}", &matrix.expr_ty)).message(
                        arm.span,
                        format!("but this pattern cannot bind a value of type {:?}", &matrix.expr_ty),
                    ),
                );
            }
        }

        if set.len() != 1 {
            return Err(Diagnostic::error(expr.span, format!("incompatible arms! {:?}", set)));
        }

        if matrix.exhaustive::<TExtKind>() {
            match set.into_iter().next() {
                Some(s) => Ok(s),
                None => Err(Diagnostic::error(
                    expr.span,
                    "probably unreachable - expected variant type!",
                )),
            }
        } else {
            Err(Diagnostic::error(expr.span, "patterns are not exhaustive!"))
        }
    }

    /// Helper function for pattern to type equivalence
    ///
    /// A `_` wildcard pattern is obviously valid for every type, as is a
    /// variable binding:
    ///     case Some(10) of
    ///         | None => None
    ///         | x => x -- x will always match to Some(10) here
    ///
    /// A literal pattern should only be equal to the equivalent type, etc
    ///
    /// This function is primarily used as a first pass to ensure that a pattern
    /// is valid for a given case expression
    pub(crate) fn pattern_type_eq<
    TExtState: Default + fmt::Debug + Clone,
    TExt: Clone + Default + fmt::Debug + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtState>,
    >(&self, pat: &ExtPattern<TExtPat>, ty: &Type, ext: &mut TExt) -> bool {
        match pat {
            ExtPattern::Any => true,
            ExtPattern::Variable(_) => true,
            ExtPattern::Literal(lit) => match (lit, ty) {
                (Literal::Bool(_), Type::Bool) => true,
                (Literal::Nat(_), Type::Nat) => true,
                (Literal::Unit, Type::Unit) => true,
                (Literal::Tag(s), Type::Tag(t)) => s == t,
                _ => false,
            },
            ExtPattern::Product(patterns) => match ty {
                Type::Product(types) => {
                    patterns.len() == types.len()
                        && patterns
                            .iter()
                            .zip(types.iter())
                            .all(|(pt, tt)| self.pattern_type_eq(pt, tt, ext))
                }
                _ => false,
            },
            ExtPattern::Constructor(label, inner) => match ty {
                Type::Variant(v) => {
                    for discriminant in v {
                        if label == &discriminant.label && self.pattern_type_eq(inner, &discriminant.ty, ext) {
                            return true;
                        }
                    }
                    false
                }
                _ => false,
            },
            ExtPattern::Extended(v) => ext.pat_ext_pattern_type_eq(v, ty),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::bottom::BottomState;

    use super::*;
    use ExtPattern::*;

    #[test]
    fn product() {
        let ty = Type::Product(vec![Type::Bool, Type::Bool, Type::Nat]);
        let pat = prod!(boolean!(true), boolean!(true), num!(10));
        let ctx = Context::default();
        let mut ext = BottomExtension;
        assert!(ctx.pattern_type_eq(&pat, &ty, &mut ext));
    }

    #[test]
    #[should_panic]
    fn product_mistyped() {
        let ty = Type::Product(vec![Type::Bool, Type::Bool, Type::Bool]);
        let pat = prod!(boolean!(true), boolean!(true), num!(10));
        let ctx = Context::default();
        let mut ext = BottomExtension;
        assert!(ctx.pattern_type_eq(&pat, &ty, &mut ext));
    }

    #[test]
    fn constructor() {
        let ty = Type::Variant(vec![
            Variant {
                label: "A".into(),
                ty: Type::Unit,
            },
            Variant {
                label: "B".into(),
                ty: Type::Nat,
            },
        ]);

        let pat1 = con!("A", ExtPattern::Any);
        let pat2 = con!("A", boolean!(true));
        let pat3 = con!("B", num!(1));

        let ctx = Context::default();
        let mut ext = BottomExtension;

        assert!(ctx.pattern_type_eq(&pat1, &ty, &mut ext));
        assert!(!ctx.pattern_type_eq(&pat2, &ty, &mut ext));
        assert!(ctx.pattern_type_eq(&pat3, &ty, &mut ext));
    }

    #[test]
    fn constructor_product() {
        let ty = Type::Variant(vec![
            Variant {
                label: "A".into(),
                ty: Type::Unit,
            },
            Variant {
                label: "B".into(),
                ty: Type::Product(vec![Type::Nat, Type::Nat]),
            },
        ]);

        let pat1 = con!("A", Any);
        let pat2 = con!("B", Any);
        let pat3 = con!("B", prod!(Any, Variable("x".into())));
        let pat4 = con!("B", prod!(num!(1), Variable("x".into())));
        let pat5 = con!("A", num!(1));

        let ctx = Context::default();
        let mut ext = BottomExtension;
        assert!(ctx.pattern_type_eq(&pat1, &ty, &mut ext));
        assert!(ctx.pattern_type_eq(&pat2, &ty, &mut ext));
        assert!(ctx.pattern_type_eq(&pat3, &ty, &mut ext));
        assert!(ctx.pattern_type_eq(&pat4, &ty, &mut ext));
        assert!(!ctx.pattern_type_eq(&pat5, &ty, &mut ext));
    }

    #[test]
    fn matrix_tuple() {
        let pats = vec![
            prod!(num!(0), num!(1)),
            prod!(num!(1), num!(1)),
            prod!(Any, num!(2)),
            prod!(num!(2), Any),
            prod!(num!(1), num!(4)),
            prod!(Any, Variable(String::default())),
        ];
        let ty = Type::Product(vec![Type::Nat, Type::Nat]);
        let mut ext = BottomExtension;
        let mut matrix = Matrix::new(ty);
        for pat in &pats {
            assert!(matrix.add_pattern::<BottomTokenKind, BottomKind, BottomState, BottomExtension>(pat, &mut ext));
        }
        assert!(!matrix.add_pattern(&Any, &mut BottomExtension));
        assert!(matrix.exhaustive::<BottomKind>());
    }

    #[test]
    fn matrix_constructor() {
        let ty = Type::Variant(vec![
            variant!("A", Type::Nat),
            variant!("B", Type::Nat),
            variant!("C", Type::Product(vec![Type::Nat, Type::Nat])),
        ]);

        let pats = vec![
            con!("A", num!(20)),
            con!("A", Any),
            con!("B", Any),
            con!("C", prod!(num!(1), num!(1))),
            con!("C", prod!(Any, num!(1))),
            con!("C", prod!(num!(1), Any)),
        ];

        let ctx = Context::default();
        assert!(pats.iter().all(|p| ctx.pattern_type_eq(p, &ty, &mut BottomExtension)));
        let mut matrix = Matrix::new(ty);

        for p in &pats {
            assert!(matrix.add_pattern(p, &mut BottomExtension));
        }
        let last = con!("C", Any);

        assert!(!matrix.exhaustive::<BottomKind>());
        assert!(matrix.add_pattern(&last, &mut BottomExtension));
        assert!(matrix.exhaustive::<BottomKind>());
    }

    #[test]
    fn matrix_bool() {
        let pats = vec![boolean!(true), boolean!(false)];

        let ty = Type::Bool;
        let ctx = Context::default();
        assert!(pats.iter().all(|p| ctx.pattern_type_eq(p, &ty, &mut BottomExtension)));

        let mut matrix = Matrix::new(ty);
        for p in &pats {
            assert!(matrix.add_pattern(p, &mut BottomExtension));
        }
        assert!(!matrix.add_pattern(&pats[1], &mut BottomExtension));
        assert!(matrix.exhaustive::<BottomKind>());
    }
}
