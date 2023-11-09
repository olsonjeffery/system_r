use core::fmt;
use std::hash;

use crate::dialect::{SystemRDialect, SystemRExtension};
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
    pub fn collect<TExt: SystemRExtension<TExtDialect> + Clone + fmt::Debug + Default>(
        pat: &Pattern<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) -> Vec<String> {
        let mut p = Self::default();
        p.visit_pattern(pat, ext, ext_state);
        p.inner
    }
}

impl<
        TExtDialect: hash::Hash + Eq + SystemRDialect + Default + fmt::Debug + Clone + PartialEq + PartialOrd,
        TExt: SystemRExtension<TExtDialect> + Clone + fmt::Debug + Default,
    > PatternVisitor<TExtDialect, TExt> for PatVarStack<TExtDialect>
{
    fn visit_variable(&mut self, var: &str, ext: &mut TExt, ext_state: &TExtDialect::TExtDialectState) {
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
    pub fn collect<TExt: SystemRExtension<TExtDialect> + Clone + fmt::Debug + Default>(
        pat: &Pattern<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) -> usize {
        let mut p = PatternCount(0, Default::default(), Default::default());
        p.visit_pattern(pat, ext, ext_state);
        p.0
    }
}

impl<
        TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd,
        TExt: SystemRExtension<TExtDialect> + Clone + fmt::Debug + Default,
    > PatternVisitor<TExtDialect, TExt> for PatternCount<TExtDialect>
{
    fn visit_variable(&mut self, var: &str, ext: &mut TExt, ext_state: &TExtDialect::TExtDialectState) {
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
pub struct PatTyStack<TExtDialect: Eq + SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd> {
    pub ty: Type<TExtDialect>,
    pub inner: Vec<Type<TExtDialect>>,
}

impl<'ty, TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + Default + fmt::Debug + PartialEq + PartialOrd>
    PatTyStack<TExtDialect>
{
    pub fn collect<TExt: SystemRExtension<TExtDialect> + Clone + fmt::Debug + Default>(
        ty: &'ty Type<TExtDialect>,
        pat: &Pattern<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) -> Vec<Type<TExtDialect>> {
        let mut p = PatTyStack {
            ty: ty.clone(),
            inner: Vec::with_capacity(16),
        };
        p.visit_pattern(pat, ext, ext_state);
        p.inner
    }
}

impl<
        TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
        TExt: SystemRExtension<TExtDialect> + fmt::Debug + Default + Clone,
    > PatternVisitor<TExtDialect, TExt> for PatTyStack<TExtDialect>
{
    fn visit_product(
        &mut self,
        pats: &Vec<Pattern<TExtDialect>>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        if let Type::Product(tys) = self.ty.clone() {
            let ty = Type::Product(tys.clone());
            for (ty, pat) in tys.iter().zip(pats.iter()) {
                self.ty = ty.clone();
                self.visit_pattern(pat, ext, ext_state);
            }
            self.ty = ty;
        }
    }

    fn visit_constructor(
        &mut self,
        label: &str,
        pat: &Pattern<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        match self.ty.clone() {
            Type::Variant(vs) => {
                let ty = self.ty.clone();
                self.ty = variant_field::<TExtDialect>(&vs, label, Span::zero()).unwrap().clone();
                self.visit_pattern(pat, ext, ext_state);
                self.ty = ty;
            }
            Type::Extended(v) => {
                ext.pat_visit_constructor_of_ext(ext_state, self, label, pat, &v);
            }
            _ => {}
        }
    }

    fn visit_ext(&mut self, p: &Pattern<TExtDialect>, ext: &mut TExt, ext_state: &TExtDialect::TExtDialectState) {}

    fn visit_pattern(
        &mut self,
        pattern: &Pattern<TExtDialect>,
        ext: &mut TExt,
        ext_state: &TExtDialect::TExtDialectState,
    ) {
        match pattern {
            Pattern::Any | Pattern::Literal(_) => {}
            Pattern::Variable(_) => self.inner.push(self.ty.clone()),
            Pattern::Constructor(label, pat) => self.visit_constructor(label, pat, ext, ext_state),
            Pattern::Product(pats) => self.visit_product(pats, ext, ext_state),
            Pattern::Extended(_) => self.visit_ext(pattern, ext, ext_state),
        }
    }
}

#[cfg(test)]
mod test {

    use crate::bottom::{BottomDialect, BottomExtension, BottomState};

    use super::*;
    #[test]
    fn pattern_count() {
        let mut pat: Pattern<BottomDialect> = Pattern::Variable(String::new());
        let mut state = BottomState;
        assert_eq!(
            PatternCount::<BottomDialect>::collect(&mut pat, &mut BottomExtension, &mut state),
            1
        );
    }

    #[test]
    fn pattern_ty_stack() {
        let mut pat: Pattern<BottomDialect> = Pattern::Variable(String::new());
        let ty = Type::Nat;
        let mut state = BottomState;
        assert_eq!(
            PatTyStack::<BottomDialect>::collect(&ty, &mut pat, &mut BottomExtension, &mut state),
            vec![ty]
        );
    }

    #[test]
    fn pattern_var_stack() {
        let mut pat: Pattern<BottomDialect> = Pattern::Variable("x".into());
        let mut state = BottomState;
        assert_eq!(
            PatVarStack::<BottomDialect>::collect(&mut pat, &mut BottomExtension, &mut state),
            vec![String::from("x")]
        );
    }
}
