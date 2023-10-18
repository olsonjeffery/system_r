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
//! Type<TExtType>checking of the simply typed lambda calculus with parametric
//! polymorphism
pub mod patterns;
pub mod visit;
use crate::bottom::{BottomDialect, BottomExtension, BottomKind, BottomPattern, BottomTokenKind, BottomType};
use crate::diagnostics::*;
use crate::extensions::{SystemRDialect, SystemRExtension};
use crate::platform_bindings::PlatformBindings;
use crate::system_r_util::span::Span;
use crate::terms::{ExtKind, ExtTerm, Literal, Primitive};
use crate::visit::{MutTermVisitor, MutTypeVisitor};
use std::collections::{HashMap, VecDeque};
use std::hash::Hash;
use std::{fmt, hash};
use visit::{Shift, Subst};

#[derive(Default, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Type<TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq> {
    #[default]
    Unit,
    Nat,
    Bool,
    Tag(String),
    Alias(String),
    Var(usize),
    Variant(Vec<Variant<TExtType>>),
    Product(Vec<Type<TExtType>>),
    PlatformBinding(Box<Type<TExtType>>, Box<Type<TExtType>>),
    Arrow(Box<Type<TExtType>>, Box<Type<TExtType>>),
    Universal(Box<Type<TExtType>>),
    Existential(Box<Type<TExtType>>),
    Rec(Box<Type<TExtType>>),
    Extended(TExtType),
}

#[derive(Debug, Default, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub struct Variant<TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq> {
    pub label: String,
    pub ty: Type<TExtType>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct TypeError<TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq> {
    pub span: Span,
    pub kind: TypeErrorKind<TExtType>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum TypeErrorKind<TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq> {
    ParameterMismatch(Box<Type<TExtType>>, Box<Type<TExtType>>, Span),

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

pub type Context = ExtContext<BottomDialect>;

#[derive(Clone, Debug, PartialEq)]
pub struct ExtContext<TExtDialect: SystemRDialect + Clone + fmt::Debug + Default> {
    stack: VecDeque<Type<TExtDialect::TExtType>>,
    map: HashMap<String, Type<TExtDialect::TExtType>>,
    pub platform_bindings: PlatformBindings,
    _d: TExtDialect,
}

impl<TExtDialect: SystemRDialect + Clone + fmt::Debug + Default> Default for ExtContext<TExtDialect> {
    fn default() -> Self {
        Self {
            stack: Default::default(),
            map: Default::default(),
            platform_bindings: Default::default(),
            _d: Default::default(),
        }
    }
}

impl<TExtDialect: SystemRDialect + PartialEq + PartialOrd + Clone + fmt::Debug + Default> ExtContext<TExtDialect> {
    fn push(&mut self, ty: Type<TExtDialect::TExtType>) {
        self.stack.push_front(ty);
    }

    fn pop(&mut self) {
        self.stack.pop_front().expect("Context::pop() with empty type stack");
    }

    fn find(&self, idx: usize) -> Option<&Type<TExtDialect::TExtType>> {
        self.stack.get(idx)
    }

    pub fn alias(&mut self, alias: String, ty: Type<TExtDialect::TExtType>) {
        self.map.insert(alias, ty);
    }

    fn aliaser(&self) -> Aliaser<'_, TExtDialect::TExtType> {
        Aliaser { map: &self.map }
    }

    pub fn de_alias(&mut self, term: &mut ExtTerm<TExtDialect>) {
        crate::visit::MutTermVisitor::visit(self, term)
    }
}

/// Helper function for extracting type from a variant
pub fn variant_field<'vs, TExtDialect: SystemRDialect + PartialEq + PartialOrd + Clone + Default + fmt::Debug>(
    var: &'vs [Variant<TExtDialect::TExtType>],
    label: &str,
    span: Span,
) -> Result<&'vs Type<TExtDialect::TExtType>, Diagnostic> {
    for f in var {
        if label == f.label {
            return Ok(&f.ty);
        }
    }
    Err(Diagnostic::error(
        span,
        format!("constructor {} doesn't appear in variant fields", label),
    ))

    // Err(Type<TExtType>Error {
    //     span,
    //     kind: Type<TExtType>ErrorKind::NotVariant,
    // })
}

impl<TExtDialect: SystemRDialect + PartialEq + PartialOrd + Clone + fmt::Debug + Default> ExtContext<TExtDialect> {
    pub fn type_check<TExt: Clone + fmt::Debug + Default + SystemRExtension<TExtDialect>>(
        &mut self,
        term: &ExtTerm<TExtDialect>,
        ext: &mut TExt,
    ) -> Result<Type<TExtDialect::TExtType>, Diagnostic> {
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
                let ty2 = self.type_check(&t2, ext)?;
                // Shift::new(-1).visit(&mut ty2);
                self.pop();
                Ok(Type::Arrow(ty.clone(), Box::new(ty2)))
            }
            ExtKind::App(t1, t2) => {
                let ty1 = self.type_check(&t1, ext)?;
                let ty2 = self.type_check(&t2, ext)?;
                match ty1.clone() {
                    Type::Arrow(ty11, ty12) => {
                        // does the invocation's type (ty2)
                        // match the type of the function's
                        // arg (ty11)?
                        if *ty11 == ty2 {
                            Ok(*ty12)
                        } else {
                            let d = Diagnostic::error(term.span, "Type<TExtType> mismatch in application")
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
                            let d =
                                Diagnostic::error(term.span, "Type<TExtType> mismatch in PlatformBinding application")
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
                let ty = self.type_check(&inner, ext)?;
                match ty {
                    Type::Arrow(ty1, ty2) => {
                        if ty1 == ty2 {
                            Ok(*ty1)
                        } else {
                            let d = Diagnostic::error(term.span, "Type<TExtType> mismatch in fix term")
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
                            let ty_ = self.type_check(&tm, ext)?;
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
            ExtKind::Projection(term, idx) => match self.type_check(&term, ext)? {
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
                terms
                    .iter()
                    .map(|t| self.type_check(t, ext))
                    .collect::<Result<_, _>>()?,
            )),
            ExtKind::Let(pat, t1, t2) => {
                let ty = self.type_check(&t1, ext)?;
                if !self.pattern_type_eq(&pat, &ty, ext) {
                    return Err(Diagnostic::error(
                        t1.span,
                        "pattern does not match type of binder".to_string(),
                    ));
                }

                let height = self.stack.len();

                let binds = crate::patterns::PatTyStack::<
                    TExtDialect,
                >::collect(&ty, pat);
                for b in binds.into_iter().rev() {
                    self.push(b.clone());
                }

                let y = self.type_check(&t2, ext);

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
                let ty2 = self.type_check(&term, ext)?;
                self.stack.iter_mut().for_each(|ty| {
                    if let Type::Var(v) = ty {
                        *v -= 1;
                    }
                });
                Ok(Type::Universal(Box::new(ty2)))
            }
            ExtKind::TyApp(term, ty) => {
                let mut ty = ty.clone();
                let ty1 = self.type_check(&term, ext)?;
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
            ExtKind::Case(expr, arms) => self.type_check_case(&expr, &arms, ext),

            ExtKind::Unfold(rec, tm) => match rec.as_ref() {
                Type::Rec(inner) => {
                    let ty_ = self.type_check(&tm, ext)?;
                    if ty_ == *rec.clone() {
                        let s = subst(*rec.clone(), *inner.clone());
                        Ok(s)
                    } else {
                        let d = Diagnostic::error(term.span, "Type<TExtType> mismatch in unfold")
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
                    let ty_ = self.type_check(&tm, ext)?;
                    let s = subst(*rec.clone(), *inner.clone());
                    if ty_ == s {
                        Ok(*rec.clone())
                    } else {
                        let d = Diagnostic::error(term.span, "Type<TExtType> mismatch in fold")
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
                    let evidence_ty = self.type_check(&evidence, ext)?;
                    if evidence_ty == sig_prime {
                        Ok(*signature.clone())
                    } else {
                        let d = Diagnostic::error(term.span, "Type<TExtType> mismatch in pack")
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
                let p_ty = self.type_check(&package, ext)?;
                if let Type::Existential(xst) = p_ty {
                    self.push(*xst);
                    let body_ty = self.type_check(&body, ext)?;
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
    pub fn type_check_ext(
        &mut self,
        t: &ExtTerm<TExtDialect>,
    ) -> Result<Type<TExtDialect::TExtType>, Diagnostic> {
        panic!("Context::type_check_ext unimplemented");
    }
}

pub fn subst<TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + Hash>(
    mut s: Type<TExtType>,
    mut t: Type<TExtType>,
) -> Type<TExtType> {
    Shift::new(1).visit(&mut s);
    Subst::new(s).visit(&mut t);
    Shift::new(-1).visit(&mut t);
    t
}

struct Aliaser<'ctx, TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq> {
    map: &'ctx HashMap<String, Type<TExtType>>,
}

impl<'ctx, TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq + hash::Hash> MutTypeVisitor<TExtType>
    for Aliaser<'ctx, TExtType>
{
    fn visit(&mut self, ty: &mut Type<TExtType>) {
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
            Type::Extended(_) => panic!("FIXME extended visitor unimplemented"),
        }
    }
}

impl<TExtDialect: SystemRDialect + PartialEq + PartialOrd + Clone + fmt::Debug + Default>
    MutTermVisitor<TExtDialect> for ExtContext<TExtDialect>
{
    fn visit_abs(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtDialect::TExtType>,
        term: &mut ExtTerm<TExtDialect>,
    ) {
        self.aliaser().visit(ty);
        self.visit(term);
    }

    fn visit_tyapp(
        &mut self,
        sp: &mut Span,
        term: &mut ExtTerm<TExtDialect>,
        ty: &mut Type<TExtDialect::TExtType>,
    ) {
        self.aliaser().visit(ty);
        self.visit(term);
    }

    fn visit_injection(
        &mut self,
        sp: &mut Span,
        label: &mut String,
        term: &mut ExtTerm<TExtDialect>,
        ty: &mut Type<TExtDialect::TExtType>,
    ) {
        self.aliaser().visit(ty);
        self.visit(term);
    }

    fn visit_fold(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtDialect::TExtType>,
        tm: &mut ExtTerm<TExtDialect>,
    ) {
        self.aliaser().visit(ty);
        self.visit(tm);
    }

    fn visit_unfold(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtDialect::TExtType>,
        tm: &mut ExtTerm<TExtDialect>,
    ) {
        self.aliaser().visit(ty);
        self.visit(tm);
    }
}

impl<TExtType: Default + Clone + fmt::Debug + PartialEq + PartialOrd + Eq> fmt::Debug for Type<TExtType> {
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
            Type::Extended(ty) => write!(f, "extended({:?})", ty),
        }
    }
}
