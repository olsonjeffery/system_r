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
//! Typechecking of the simply typed lambda calculus with parametric
//! polymorphism
pub mod patterns;
pub mod visit;
use crate::bottom::{BottomExtension, BottomKind, BottomPattern, BottomTokenKind};
use crate::diagnostics::*;
use crate::extensions::SystemRExtension;
use crate::platform_bindings::PlatformBindings;
use crate::system_r_util::span::Span;
use crate::terms::{ExtKind, ExtTerm, Literal, Primitive};
use crate::visit::{MutTermVisitor, MutTypeVisitor};
use std::collections::{HashMap, VecDeque};
use std::fmt;
use visit::{Shift, Subst};

#[derive(Default, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Type {
    #[default]
    Unit,
    Nat,
    Bool,
    Tag(String),
    Alias(String),
    Var(usize),
    Variant(Vec<Variant>),
    Product(Vec<Type>),
    PlatformBinding(Box<Type>, Box<Type>),
    Arrow(Box<Type>, Box<Type>),
    Universal(Box<Type>),
    Existential(Box<Type>),
    Rec(Box<Type>),
}

#[derive(Debug, Default, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub struct Variant {
    pub label: String,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct TypeError {
    pub span: Span,
    pub kind: TypeErrorKind,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum TypeErrorKind {
    ParameterMismatch(Box<Type>, Box<Type>, Span),

    InvalidProjection,
    NotArrow,
    NotUniversal,
    NotVariant,
    NotProduct,
    NotRec,
    IncompatibleArms,
    InvalidPattern,
    NotExhaustive,
    UnreachablePattern,
    UnboundVariable(usize),
}

pub type Context = ExtContext<BottomTokenKind, BottomKind, BottomPattern>;

#[derive(Clone, Debug, PartialEq)]
pub struct ExtContext<
    TExtTokenKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    TExtKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    TExtPat: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
> {
    stack: VecDeque<Type>,
    map: HashMap<String, Type>,
    pub platform_bindings: PlatformBindings,
    _token: TExtTokenKind,
    _kind: TExtKind,
    _pat: TExtPat,
}

impl<
        TExtTokenKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
        TExtKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
        TExtPat: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    > Default for ExtContext<TExtTokenKind, TExtKind, TExtPat>
{
    fn default() -> Self {
        Self {
            stack: Default::default(),
            map: Default::default(),
            platform_bindings: Default::default(),
            _token: Default::default(),
            _kind: Default::default(),
            _pat: Default::default(),
        }
    }
}

impl<
        TExtTokenKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
        TExtKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
        TExtPat: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    > ExtContext<TExtTokenKind, TExtKind, TExtPat>
{
    fn push(&mut self, ty: Type) {
        self.stack.push_front(ty);
    }

    fn pop(&mut self) {
        self.stack.pop_front().expect("Context::pop() with empty type stack");
    }

    fn find(&self, idx: usize) -> Option<&Type> {
        self.stack.get(idx)
    }

    pub fn alias(&mut self, alias: String, ty: Type) {
        self.map.insert(alias, ty);
    }

    fn aliaser(&self) -> Aliaser<'_> {
        Aliaser { map: &self.map }
    }

    pub fn de_alias(&mut self, term: &mut ExtTerm<TExtPat, TExtKind>) {
        crate::visit::MutTermVisitor::visit(self, term)
    }
}

/// Helper function for extracting type from a variant
pub fn variant_field<'vs>(var: &'vs [Variant], label: &str, span: Span) -> Result<&'vs Type, Diagnostic> {
    for f in var {
        if label == f.label {
            return Ok(&f.ty);
        }
    }
    Err(Diagnostic::error(
        span,
        format!("constructor {} doesn't appear in variant fields", label),
    ))

    // Err(TypeError {
    //     span,
    //     kind: TypeErrorKind::NotVariant,
    // })
}

impl<
        TExtTokenKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
        TExtKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
        TExtPat: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    > ExtContext<TExtTokenKind, TExtKind, TExtPat>
{
    pub fn type_check<
    TExtState: Clone + Default + fmt::Debug,
    TExt: Clone + fmt::Debug + Default + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtState>,
    >(&mut self, term: &ExtTerm<TExtPat, TExtKind>, ext: &mut TExt) -> Result<Type, Diagnostic> {
        // dbg!(&self.stack);

        match term.kind() {
            ExtKind::Extended(_) => self.type_check_ext(term),
            ExtKind::Lit(Literal::Unit) => Ok(Type::Unit),
            ExtKind::Lit(Literal::Bool(_)) => Ok(Type::Bool),
            ExtKind::Lit(Literal::Nat(_)) => Ok(Type::Nat),
            ExtKind::Lit(Literal::Tag(s)) => Ok(Type::Tag(s.clone())),
            ExtKind::PlatformBinding(idx) => self.type_check_platform_binding(idx, &term.span),
            ExtKind::Var(idx) => self
                .find(*idx)
                .cloned()
                .ok_or_else(|| Diagnostic::error(term.span, format!("type_check: unbound variable {}", idx))),

            ExtKind::Abs(ty, t2) => {
                self.push(*ty.clone());
                let ty2 = self.type_check(t2, ext)?;
                // Shift::new(-1).visit(&mut ty2);
                self.pop();
                Ok(Type::Arrow(ty.clone(), Box::new(ty2)))
            }
            ExtKind::App(t1, t2) => {
                let ty1 = self.type_check(t1, ext)?;
                let ty2 = self.type_check(t2, ext)?;
                match ty1.clone() {
                    Type::Arrow(ty11, ty12) => {
                        // does the invocation's type (ty2)
                        // match the type of the function's
                        // arg (ty11)?
                        if *ty11 == ty2 {
                            Ok(*ty12)
                        } else {
                            let d = Diagnostic::error(term.span, "Type mismatch in application")
                                .message(t1.span, format!("Abstraction requires type {:?}", ty11))
                                .message(t2.span, format!("Value has a type of {:?}", ty2));
                            Err(d)
                        }
                    }
                    Type::PlatformBinding(ty11, ty12) => {
                        // repeat Arrow logic above
                        if *ty11 == ty2 {
                            Ok(*ty12)
                        } else {
                            let d = Diagnostic::error(term.span, "Type mismatch in PlatformBinding application")
                                .message(t1.span, format!("PlatformBinding Abstraction requires type {:?}", ty11))
                                .message(t2.span, format!("Value has a type of {:?}", ty2));
                            Err(d)
                        }
                    }
                    _ => Err(Diagnostic::error(term.span, "app: Expected arrow type!")
                        .message(t1.span, format!("operator has type {:?}", ty1))),
                }
            }
            ExtKind::Fix(inner) => {
                let ty = self.type_check(inner, ext)?;
                match ty {
                    Type::Arrow(ty1, ty2) => {
                        if ty1 == ty2 {
                            Ok(*ty1)
                        } else {
                            let d = Diagnostic::error(term.span, "Type mismatch in fix term")
                                .message(inner.span, format!("Abstraction requires type {:?}->{:?}", ty1, ty1));
                            Err(d)
                        }
                    }
                    _ => Err(Diagnostic::error(term.span, "Fix: Expected arrow type!")
                        .message(inner.span, format!("operator has type {:?}", ty))),
                }
            }
            ExtKind::Primitive(prim) => match prim {
                Primitive::IsZero => Ok(Type::Arrow(Box::new(Type::Nat), Box::new(Type::Bool))),
                _ => Ok(Type::Arrow(Box::new(Type::Nat), Box::new(Type::Nat))),
            },
            ExtKind::Injection(label, tm, ty) => match ty.as_ref() {
                Type::Variant(fields) => {
                    for f in fields {
                        if label == &f.label {
                            let ty_ = self.type_check(tm, ext)?;
                            if ty_ == f.ty {
                                return Ok(*ty.clone());
                            } else {
                                let d = Diagnostic::error(term.span, "Invalid associated type in variant").message(
                                    tm.span,
                                    format!("variant {} requires type {:?}, but this is {:?}", label, f.ty, ty_),
                                );
                                return Err(d);
                            }
                        }
                    }
                    Err(Diagnostic::error(
                        term.span,
                        format!(
                            "constructor {} does not belong to the variant {:?}",
                            label,
                            fields
                                .iter()
                                .map(|f| f.label.clone())
                                .collect::<Vec<String>>()
                                .join(" | ")
                        ),
                    ))
                }
                _ => Err(Diagnostic::error(
                    term.span,
                    format!("Cannot injection {} into non-variant type {:?}", label, ty),
                )),
            },
            ExtKind::Projection(term, idx) => match self.type_check(term, ext)? {
                Type::Product(types) => match types.get(*idx) {
                    Some(ty) => Ok(ty.clone()),
                    None => Err(Diagnostic::error(
                        term.span,
                        format!("{} is out of range for product of length {}", idx, types.len()),
                    )),
                },
                ty => Err(Diagnostic::error(
                    term.span,
                    format!("Cannot project on non-product type {:?}", ty),
                )),
            },
            ExtKind::Product(terms) => Ok(Type::Product(
                terms.iter().map(|t| self.type_check(t, ext)).collect::<Result<_, _>>()?,
            )),
            ExtKind::Let(pat, t1, t2) => {
                let ty = self.type_check(t1, ext)?;
                if !self.pattern_type_eq(pat, &ty, ext) {
                    return Err(Diagnostic::error(
                        t1.span,
                        "pattern does not match type of binder".to_string(),
                    ));
                }

                let height = self.stack.len();

                let binds = crate::patterns::PatTyStack::<TExtPat, TExtKind>::collect(&ty, pat);
                for b in binds.into_iter().rev() {
                    self.push(b.clone());
                }

                let y = self.type_check(t2, ext);

                while self.stack.len() > height {
                    self.pop();
                }

                y
            }
            ExtKind::TyAbs(term) => {
                self.stack.iter_mut().for_each(|ty| {
                    if let Type::Var(v) = ty {
                        *v += 1;
                    }
                });
                let ty2 = self.type_check(term, ext)?;
                self.stack.iter_mut().for_each(|ty| {
                    if let Type::Var(v) = ty {
                        *v -= 1;
                    }
                });
                Ok(Type::Universal(Box::new(ty2)))
            }
            ExtKind::TyApp(term, ty) => {
                let mut ty = ty.clone();
                let ty1 = self.type_check(term, ext)?;
                match ty1 {
                    Type::Universal(mut ty12) => {
                        Shift::new(1).visit(&mut ty);
                        Subst::new(*ty).visit(&mut ty12);
                        Shift::new(-1).visit(&mut ty12);
                        Ok(*ty12)
                    }
                    _ => Err(Diagnostic::error(
                        term.span,
                        format!("Expected a universal type, not {:?}", ty1),
                    )),
                }
            }
            // See src/types/patterns.rs for exhaustiveness and typechecking
            // of case expressions
            ExtKind::Case(expr, arms) => self.type_check_case(expr, arms, ext),

            ExtKind::Unfold(rec, tm) => match rec.as_ref() {
                Type::Rec(inner) => {
                    let ty_ = self.type_check(tm, ext)?;
                    if ty_ == *rec.clone() {
                        let s = subst(*rec.clone(), *inner.clone());
                        Ok(s)
                    } else {
                        let d = Diagnostic::error(term.span, "Type mismatch in unfold")
                            .message(term.span, format!("unfold requires type {:?}", rec))
                            .message(tm.span, format!("term has a type of {:?}", ty_));
                        Err(d)
                    }
                }
                _ => Err(Diagnostic::error(
                    term.span,
                    format!("Expected a recursive type, not {:?}", rec),
                )),
            },

            ExtKind::Fold(rec, tm) => match rec.as_ref() {
                Type::Rec(inner) => {
                    let ty_ = self.type_check(tm, ext)?;
                    let s = subst(*rec.clone(), *inner.clone());
                    if ty_ == s {
                        Ok(*rec.clone())
                    } else {
                        let d = Diagnostic::error(term.span, "Type mismatch in fold")
                            .message(term.span, format!("unfold requires type {:?}", s))
                            .message(tm.span, format!("term has a type of {:?}", ty_));
                        Err(d)
                    }
                }
                _ => Err(Diagnostic::error(
                    term.span,
                    format!("Expected a recursive type, not {:?}", rec),
                )),
            },
            ExtKind::Pack(witness, evidence, signature) => {
                if let Type::Existential(exists) = signature.as_ref() {
                    let sig_prime = subst(*witness.clone(), *exists.clone());
                    let evidence_ty = self.type_check(evidence, ext)?;
                    if evidence_ty == sig_prime {
                        Ok(*signature.clone())
                    } else {
                        let d = Diagnostic::error(term.span, "Type mismatch in pack")
                            .message(term.span, format!("signature has type {:?}", sig_prime))
                            .message(evidence.span, format!("but term has a type {:?}", evidence_ty));
                        Err(d)
                    }
                } else {
                    Err(Diagnostic::error(
                        term.span,
                        format!("Expected an existential type signature, not {:?}", signature),
                    ))
                }
            }
            ExtKind::Unpack(package, body) => {
                let p_ty = self.type_check(package, ext)?;
                if let Type::Existential(xst) = p_ty {
                    self.push(*xst);
                    let body_ty = self.type_check(body, ext)?;
                    self.pop();
                    Ok(body_ty)
                } else {
                    Err(Diagnostic::error(
                        package.span,
                        format!("Expected an existential type signature, not {:?}", p_ty),
                    ))
                }
            }
        }
    }
    pub fn type_check_ext(&mut self, t: &ExtTerm<TExtPat, TExtKind>) -> Result<Type, Diagnostic> {
        panic!("Context::type_check_ext unimplemented");
    }
}

pub fn subst(mut s: Type, mut t: Type) -> Type {
    Shift::new(1).visit(&mut s);
    Subst::new(s).visit(&mut t);
    Shift::new(-1).visit(&mut t);
    t
}

struct Aliaser<'ctx> {
    map: &'ctx HashMap<String, Type>,
}

impl<'ctx> MutTypeVisitor for Aliaser<'ctx> {
    fn visit(&mut self, ty: &mut Type) {
        match ty {
            Type::Unit | Type::Bool | Type::Nat | Type::Tag(_) => {}
            Type::Var(v) => {}
            Type::PlatformBinding(i, r) => {}
            Type::Alias(v) => {
                if let Some(aliased) = self.map.get(v) {
                    *ty = aliased.clone();
                }
            }
            Type::Variant(v) => self.visit_variant(v),
            Type::Product(v) => self.visit_product(v),

            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2),
            Type::Universal(ty) => self.visit_universal(ty),
            Type::Existential(ty) => self.visit_existential(ty),
            Type::Rec(ty) => self.visit_rec(ty),
        }
    }
}

impl<
        TExtTokenKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
        TExtKind: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
        TExtPat: Clone + Default + fmt::Debug + PartialEq + PartialOrd,
    > MutTermVisitor<TExtPat, TExtKind> for ExtContext<TExtTokenKind, TExtKind, TExtPat>
{
    fn visit_abs(&mut self, sp: &mut Span, ty: &mut Type, term: &mut ExtTerm<TExtPat, TExtKind>) {
        self.aliaser().visit(ty);
        self.visit(term);
    }

    fn visit_tyapp(&mut self, sp: &mut Span, term: &mut ExtTerm<TExtPat, TExtKind>, ty: &mut Type) {
        self.aliaser().visit(ty);
        self.visit(term);
    }

    fn visit_injection(
        &mut self,
        sp: &mut Span,
        label: &mut String,
        term: &mut ExtTerm<TExtPat, TExtKind>,
        ty: &mut Type,
    ) {
        self.aliaser().visit(ty);
        self.visit(term);
    }

    fn visit_fold(&mut self, sp: &mut Span, ty: &mut Type, tm: &mut ExtTerm<TExtPat, TExtKind>) {
        self.aliaser().visit(ty);
        self.visit(tm);
    }

    fn visit_unfold(&mut self, sp: &mut Span, ty: &mut Type, tm: &mut ExtTerm<TExtPat, TExtKind>) {
        self.aliaser().visit(ty);
        self.visit(tm);
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Bool => write!(f, "Bool"),
            Type::Nat => write!(f, "Nat"),
            Type::Tag(s) => write!(f, "Tag({})", s),
            Type::PlatformBinding(i, r) => write!(f, "PlatformBinding({:?}, {:?})", i, r),
            Type::Var(v) => write!(f, "TyVar({})", v),
            Type::Variant(v) => write!(
                f,
                "{:?}",
                v.iter()
                    .map(|x| format!("{}: {:?}", x.label, x.ty))
                    .collect::<Vec<String>>()
                    .join(" | ")
            ),
            Type::Product(v) => write!(
                f,
                "({})",
                v.iter().map(|x| format!("{:?}", x)).collect::<Vec<String>>().join(",")
            ),
            Type::Alias(s) => write!(f, "{}", s),
            Type::Arrow(t1, t2) => write!(f, "({:?}->{:?})", t1, t2),
            Type::Universal(ty) => write!(f, "forall X.{:?}", ty),
            Type::Existential(ty) => write!(f, "exists X.{:?}", ty),
            Type::Rec(ty) => write!(f, "rec {:?}", ty),
        }
    }
}
