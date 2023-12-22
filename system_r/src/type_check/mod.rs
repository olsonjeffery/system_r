//! Type checking of the simply typed lambda calculus with
//! parametric polymorphism
pub mod patterns;
pub mod visit;
pub mod error;
use crate::type_check::error::*;
use crate::dialect::{SystemRDialect, SystemRExtension};
use crate::platform_bindings::Bindings;
use crate::util::span::Span;
use crate::terms::{Kind, Literal, Primitive, Term};
use crate::visit::{MutTermVisitor, MutTypeVisitor};
use anyhow::Result;
use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::hash::Hash;
use visit::{Shift, Subst};

#[derive(Default, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Type<TExtDialect: SystemRDialect> {
    #[default]
    Unit,
    Nat,
    Bool,
    Bytes,
    Tag(String),
    Alias(String),
    Var(usize),
    Variant(Vec<Variant<TExtDialect>>),
    Product(Vec<Type<TExtDialect>>),
    PlatformBinding(Box<Type<TExtDialect>>, Box<Type<TExtDialect>>),
    Arrow(Box<Type<TExtDialect>>, Box<Type<TExtDialect>>),
    Universal(Box<Type<TExtDialect>>),
    Existential(Box<Type<TExtDialect>>),
    Rec(Box<Type<TExtDialect>>),
    Extended(TExtDialect::Type),
}

#[derive(Debug, Default, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub struct Variant<TExtDialect: SystemRDialect> {
    pub label: String,
    pub ty: Type<TExtDialect>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeChecker<TExtDialect: SystemRDialect> {
    pub stack: VecDeque<Type<TExtDialect>>,
    pub map: HashMap<String, Type<TExtDialect>>,
    pub platform_bindings: Bindings,
    pub ext_state: TExtDialect::DialectState,
}

impl<TExtDialect: SystemRDialect> Default for TypeChecker<TExtDialect> {
    fn default() -> Self {
        Self {
            stack: Default::default(),
            map: Default::default(),
            platform_bindings: Default::default(),
            ext_state: Default::default(),
        }
    }
}

impl<TExtDialect: SystemRDialect> TypeChecker<TExtDialect> {
    fn push(&mut self, ty: Type<TExtDialect>) {
        self.stack.push_front(ty);
    }

    fn pop(&mut self) {
        self.stack.pop_front().expect("Context::pop() with empty type stack");
    }

    pub fn find(&self, idx: usize) -> Option<&Type<TExtDialect>> {
        self.stack.get(idx)
    }

    pub fn alias(&mut self, alias: String, ty: Type<TExtDialect>) {
        self.map.insert(alias, ty);
    }

    fn aliaser(&self) -> Aliaser<'_, TExtDialect> {
        Aliaser { map: &self.map }
    }

    pub fn de_alias<TExt: SystemRExtension<TExtDialect>>(&mut self, term: &mut Term<TExtDialect>, ext: &mut TExt) {
        match crate::visit::MutTermVisitor::visit(self, term, ext, &self.ext_state.clone()) {
            Ok(t) => t,
            Err(e) => panic!("unimpl panic handler {:?}", e),
        }
    }

    pub fn to_ext_state(self) -> TExtDialect::DialectState {
        self.ext_state
    }
}

/// Helper function for extracting type from a variant
pub fn variant_field<'vs, TExtDialect: SystemRDialect>(
    var: &'vs [Variant<TExtDialect>],
    label: &str,
    span: Span,
) -> Result<&'vs Type<TExtDialect>> {
    for f in var {
        if label == f.label {
            return Ok(&f.ty);
        }
    }
    Err(TypeCheckerDiagnosticInfo::error(span, format!("constructor {} doesn't appear in variant fields", label)).into())
}

impl<TExtDialect: SystemRDialect> TypeChecker<TExtDialect> {
    pub fn type_check<TExt: SystemRExtension<TExtDialect>>(
        &mut self,
        term: &Term<TExtDialect>,
        ext: &mut TExt,
    ) -> Result<Type<TExtDialect>> {
        match term.kind() {
            Kind::Extended(_) => self.type_check_ext(term),
            Kind::Lit(Literal::Bytes(_)) => Ok(Type::Bytes),
            Kind::Lit(Literal::Unit) => Ok(Type::Unit),
            Kind::Lit(Literal::Bool(_)) => Ok(Type::Bool),
            Kind::Lit(Literal::Nat(_)) => Ok(Type::Nat),
            Kind::Lit(Literal::Tag(s)) => Ok(Type::Tag(s.clone())),
            Kind::PlatformBinding(idx) => self.type_check_platform_binding(idx, &term.span),
            Kind::Var(idx) => {
                let result = self
                    .find(*idx)
                    .cloned()
                    .ok_or_else(|| TypeCheckerDiagnosticInfo::error(term.span, format!("type_check: unbound variable {}", idx)))?;
                Ok(result)
            }

            Kind::Abs(ty, t2) => {
                self.push(*ty.clone());
                let ty2 = self.type_check(t2, ext)?;
                // Shift::new(-1).visit(&mut ty2);
                self.pop();
                Ok(Type::Arrow(ty.clone(), Box::new(ty2)))
            }
            Kind::App(t1, t2) => {
                let ty1 = self.type_check(t1, ext)?;
                let mut ty2 = self.type_check(t2, ext)?;
                match ty1.clone() {
                    Type::Arrow(ty11, ty12) => {
                        // does the invocation's type (ty2)
                        // match the type of the function's
                        // arg (ty11)?
                        if *ty11 == ty2 {
                            Ok(*ty12)
                        } else {
                            match *ty11.clone() {
                                Type::Extended(ext_ty) => {
                                    // essentially a recapitulation of check above
                                    if ext.type_check_ext_equals_ty(self, &mut ext_ty.clone(), &mut ty2) {
                                        Ok(*ty12)
                                    } else {
                                        let d = TypeCheckerDiagnosticInfo::error(
                                            term.span,
                                            "Type<TExtDialect> mismatch in ext application, ty11 side",
                                        )
                                        .message(t2.span, format!("WTF is ty12? {:?}", ty12))
                                        .message(term.span, format!("parent term {:?}", term.kind))
                                        .message(term.span, format!("ctx stack {:?}", self.stack));
                                        Err(d.into())
                                    }
                                }
                                _ => {
                                    let d = TypeCheckerDiagnosticInfo::error(term.span, "Type<TExtDialect> mismatch in application")
                                        .message(t1.span, format!("Abstraction requires type {:?}", ty11))
                                        .message(t2.span, format!("Value has a type of {:?}", ty2))
                                        .message(t1.span, format!("Stack contents: {:?}", self.stack))
                                        .message(term.span, format!("parent term {:?}", term.kind));
                                    Err(d.into())
                                }
                            }
                        }
                    }
                    Type::PlatformBinding(ty11, ty12) => {
                        // repeat Arrow logic above
                        if *ty11 == ty2 {
                            Ok(*ty12)
                        } else {
                            let d = TypeCheckerDiagnosticInfo::error(
                                term.span,
                                "Type<TExtDialect> mismatch in PlatformBinding application",
                            )
                            .message(t1.span, format!("PlatformBinding Abstraction requires type {:?}", ty11))
                            .message(t2.span, format!("Value has a type of {:?}", ty2));
                            Err(d.into())
                        }
                    }
                    Type::Extended(t) => {
                        panic!("SHOULDNT HAPPEN; type_check on Kind::App value with an extended in t1; term: {:?} span: {:?} t: {:?} ty+stack {:?}", term, term.span.clone(), t, self.stack);
                        //ext.type_check_application_of_ext(self, t1, &ty1, t2,
                        // &ty2)
                    }
                    _ => Err(TypeCheckerDiagnosticInfo::error(term.span, "App: Expected arrow type!")
                        .message(t1.span, format!("Kind::App ty1 {:?} ty2 {:?}", &ty1, &ty2))
                        .into()),
                }
            }
            Kind::Fix(inner) => {
                let ty = self.type_check(inner, ext)?;
                match ty {
                    Type::Arrow(ty1, ty2) => {
                        if ty1 == ty2 {
                            Ok(*ty1)
                        } else {
                            let d = TypeCheckerDiagnosticInfo::error(term.span, "Type<TExtDialect> mismatch in fix term")
                                .message(inner.span, format!("Abstraction requires type {:?}->{:?}", ty1, ty1));
                            Err(d.into())
                        }
                    }
                    _ => Err(TypeCheckerDiagnosticInfo::error(term.span, "Fix: Expected arrow type!")
                        .message(
                            inner.span,
                            format!("Kind::Fix -> type_check operator has type {:?}", ty),
                        )
                        .into()),
                }
            }
            Kind::Primitive(prim) => match prim {
                Primitive::IsZero => Ok(Type::Arrow(Box::new(Type::Nat), Box::new(Type::Bool))),
                _ => Ok(Type::Arrow(Box::new(Type::Nat), Box::new(Type::Nat))),
            },
            Kind::Injection(label, tm, ty) => match ty.as_ref() {
                Type::Variant(fields) => {
                    for f in fields {
                        if label == &f.label {
                            let ty_ = self.type_check(tm, ext)?;
                            if ty_ == f.ty {
                                return Ok(*ty.clone());
                            } else {
                                let d = TypeCheckerDiagnosticInfo::error(term.span, "Invalid associated type in variant").message(
                                    tm.span,
                                    format!("variant {} requires type {:?}, but this is {:?}", label, f.ty, ty_),
                                );
                                return Err(d.into());
                            }
                        }
                    }
                    Err(TypeCheckerDiagnosticInfo::error(
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
                    )
                    .into())
                }
                Type::Extended(t) => ext.type_check_injection_to_ext(self, label, t, tm),

                _ => Err(TypeCheckerDiagnosticInfo::error(
                    term.span,
                    format!("Cannot injection {} into non-variant type {:?}", label, ty),
                )
                .into()),
            },
            Kind::Projection(term, idx) => match self.type_check(term, ext)? {
                Type::Product(types) => match types.get(*idx) {
                    Some(ty) => Ok(ty.clone()),
                    None => Err(TypeCheckerDiagnosticInfo::error(
                        term.span,
                        format!("{} is out of range for product of length {}", idx, types.len()),
                    )
                    .into()),
                },
                ty => Err(TypeCheckerDiagnosticInfo::error(term.span, format!("Cannot project on non-product type {:?}", ty)).into()),
            },
            Kind::Product(terms) => Ok(Type::Product(
                terms
                    .iter()
                    .map(|t| self.type_check(t, ext))
                    .collect::<Result<_, _>>()?,
            )),
            Kind::Let(pat, t1, t2) => {
                let ty = self.type_check(t1, ext)?;
                if !self.pattern_type_eq(pat, &ty, ext) {
                    return Err(TypeCheckerDiagnosticInfo::error(t1.span, "pattern does not match type of binder".to_string()).into());
                }

                let height = self.stack.len();

                let binds = crate::patterns::PatTyStack::<TExtDialect>::collect(&ty, pat, ext, &self.ext_state);
                for b in binds.into_iter().rev() {
                    self.push(b.clone());
                }

                let y = self.type_check(t2, ext);

                while self.stack.len() > height {
                    self.pop();
                }

                y
            }
            Kind::TyAbs(term) => {
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
            Kind::TyApp(term, ty) => {
                let mut ty = ty.clone();
                let ty1 = self.type_check(term, ext)?;
                match ty1 {
                    Type::Universal(mut ty12) => {
                        if let Err(e) = Shift::new(1).visit(&mut ty, ext, &self.ext_state) {
                            panic!("need result here {:?}", e)
                        }
                        if let Err(e) = Subst::new(*ty.clone()).visit(&mut ty12, ext, &self.ext_state) {
                            panic!("need result here {:?}", e)
                        }
                        if let Err(e) = Shift::new(-1).visit(&mut ty12, ext, &self.ext_state) {
                            panic!("need result here {:?}", e)
                        }
                        Ok(*ty12)
                    }
                    _ => Err(TypeCheckerDiagnosticInfo::error(term.span, format!("Expected a universal type, not {:?}", ty1)).into()),
                }
            }
            // See src/types/patterns.rs for exhaustiveness and typechecking
            // of case expressions
            Kind::Case(expr, arms) => self.type_check_case(expr, arms, ext),

            Kind::Unfold(rec, tm) => match rec.as_ref() {
                Type::Rec(inner) => {
                    let ty_ = self.type_check(tm, ext)?;
                    if ty_ == *rec.clone() {
                        let s = subst(*rec.clone(), *inner.clone(), ext, &self.ext_state);
                        Ok(s)
                    } else {
                        let d = TypeCheckerDiagnosticInfo::error(term.span, "Type<TExtDialect> mismatch in unfold")
                            .message(term.span, format!("unfold requires type {:?}", rec))
                            .message(tm.span, format!("term has a type of {:?}", ty_));
                        Err(d.into())
                    }
                }
                _ => Err(TypeCheckerDiagnosticInfo::error(term.span, format!("Expected a recursive type, not {:?}", rec)).into()),
            },

            Kind::Fold(rec, tm) => match rec.as_ref() {
                Type::Rec(inner) => {
                    let ty_ = self.type_check(tm, ext)?;
                    let s = subst(*rec.clone(), *inner.clone(), ext, &self.ext_state);
                    if ty_ == s {
                        Ok(*rec.clone())
                    } else {
                        let d = TypeCheckerDiagnosticInfo::error(term.span, "Type<TExtDialect> mismatch in fold")
                            .message(term.span, format!("unfold requires type {:?}", s))
                            .message(tm.span, format!("term has a type of {:?}", ty_));
                        Err(d.into())
                    }
                }
                _ => Err(TypeCheckerDiagnosticInfo::error(term.span, format!("Expected a recursive type, not {:?}", rec)).into()),
            },
            Kind::Pack(witness, evidence, signature) => {
                if let Type::Existential(exists) = signature.as_ref() {
                    let sig_prime = subst(*witness.clone(), *exists.clone(), ext, &self.ext_state);
                    let evidence_ty = self.type_check(evidence, ext)?;
                    if evidence_ty == sig_prime {
                        Ok(*signature.clone())
                    } else {
                        let d = TypeCheckerDiagnosticInfo::error(term.span, "Type<TExtDialect> mismatch in pack")
                            .message(term.span, format!("signature has type {:?}", sig_prime))
                            .message(evidence.span, format!("but term has a type {:?}", evidence_ty));
                        Err(d.into())
                    }
                } else {
                    Err(TypeCheckerDiagnosticInfo::error(
                        term.span,
                        format!("Expected an existential type signature, not {:?}", signature),
                    )
                    .into())
                }
            }
            Kind::Unpack(package, body) => {
                let p_ty = self.type_check(package, ext)?;
                if let Type::Existential(xst) = p_ty {
                    self.push(*xst);
                    let body_ty = self.type_check(body, ext)?;
                    self.pop();
                    Ok(body_ty)
                } else {
                    Err(TypeCheckerDiagnosticInfo::error(
                        package.span,
                        format!("Expected an existential type signature, not {:?}", p_ty),
                    )
                    .into())
                }
            }
        }
    }
    pub fn type_check_ext(&mut self, t: &Term<TExtDialect>) -> Result<Type<TExtDialect>> {
        panic!("Context::type_check_ext unimplemented");
    }
}

pub fn subst<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    mut s: Type<TExtDialect>,
    mut t: Type<TExtDialect>,
    ext: &mut TExt,
    ext_state: &TExtDialect::DialectState,
) -> Type<TExtDialect> {
    if let Err(e) = Shift::new(1).visit(&mut s, ext, ext_state) {
        panic!("need result here {:?}", e)
    }
    if let Err(e) = Subst::new(s).visit(&mut t, ext, ext_state) {
        panic!("need result here {:?}", e)
    }
    if let Err(e) = Shift::new(-1).visit(&mut t, ext, ext_state) {
        panic!("need result here {:?}", e)
    }
    t
}

pub struct Aliaser<'ctx, TExtDialect: SystemRDialect> {
    map: &'ctx HashMap<String, Type<TExtDialect>>,
}

impl<'ctx, TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>> MutTypeVisitor<TExtDialect, TExt>
    for Aliaser<'ctx, TExtDialect>
{
    fn visit_ext(
        &mut self,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &<TExtDialect as SystemRDialect>::DialectState,
    ) -> Result<()> {
        ext.ty_aliaser_visit_ext(self, ty, ext_state);
        Ok(())
    }

    fn visit(
        &mut self,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()> {
        match ty {
            Type::Unit | Type::Bool | Type::Nat | Type::Tag(_) | Type::Bytes => Ok(()),
            Type::Var(v) => Ok(()),
            Type::PlatformBinding(i, r) => Ok(()),
            Type::Alias(v) => {
                if let Some(aliased) = self.map.get(v) {
                    *ty = aliased.clone();
                }
                Ok(())
            }
            Type::Variant(v) => self.visit_variant(v, ext, ext_state),
            Type::Product(v) => self.visit_product(v, ext, ext_state),

            Type::Arrow(ty1, ty2) => self.visit_arrow(ty1, ty2, ext, ext_state),
            Type::Universal(ty) => self.visit_universal(ty, ext, ext_state),
            Type::Existential(ty) => self.visit_existential(ty, ext, ext_state),
            Type::Rec(ty) => self.visit_rec(ty, ext, ext_state),
            Type::Extended(_) => self.visit_ext(ty, ext, ext_state),
        }
    }
}

impl<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>> MutTermVisitor<TExtDialect, TExt>
    for TypeChecker<TExtDialect>
{
    fn visit_abs(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtDialect>,
        term: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()> {
        self.aliaser().visit(ty, ext, ext_state)?;
        self.visit(term, ext, ext_state)
    }

    fn visit_tyapp(
        &mut self,
        sp: &mut Span,
        term: &mut Term<TExtDialect>,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()> {
        self.aliaser().visit(ty, ext, ext_state)?;
        self.visit(term, ext, ext_state)
    }

    fn visit_injection(
        &mut self,
        sp: &mut Span,
        label: &mut String,
        term: &mut Term<TExtDialect>,
        ty: &mut Type<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()> {
        self.aliaser().visit(ty, ext, ext_state)?;
        self.visit(term, ext, ext_state)
    }

    fn visit_fold(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtDialect>,
        tm: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()> {
        self.aliaser().visit(ty, ext, ext_state)?;
        self.visit(tm, ext, ext_state)
    }

    fn visit_unfold(
        &mut self,
        sp: &mut Span,
        ty: &mut Type<TExtDialect>,
        tm: &mut Term<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::DialectState,
    ) -> Result<()> {
        self.aliaser().visit(ty, ext, ext_state)?;
        self.visit(tm, ext, ext_state)
    }

    fn visit_ext(
        &mut self,
        sp: &mut Span,
        k: &<TExtDialect as SystemRDialect>::Kind,
        ext: &mut TExt,
        ext_state: &<TExtDialect as SystemRDialect>::DialectState,
    ) -> Result<()> {
        Err(anyhow!("not implemented"))
    }
}

impl<TExtDialect: SystemRDialect> fmt::Debug for Type<TExtDialect> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Bytes => write!(f, "Bytes"),
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
