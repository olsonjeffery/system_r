//! Representation lambda calculus terms
use crate::dialect::SystemRDialect;
use crate::patterns::Pattern;
use crate::type_check::Type;
use crate::util::span::Span;
use std::fmt;
pub mod plaintext;
pub mod visit;

#[derive(Clone, Default, PartialEq, PartialOrd)]
pub struct Term<TExtDialect: SystemRDialect> {
    pub span: Span,
    pub kind: Kind<TExtDialect>,
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
pub enum Kind<TExtDialect: SystemRDialect> {
    /// A literal value
    Lit(Literal),
    /// A bound variable, represented by it's de Bruijn index
    Var(usize),
    /// A re
    PlatformBinding(usize),
    /// An intrinsic, referenced by alias
    /// Fixpoint operator/Y combinator
    Fix(Box<Term<TExtDialect>>),

    Primitive(Primitive),

    /// Injection into a sum type
    /// fields: type constructor tag, term, and sum type
    Injection(String, Box<Term<TExtDialect>>, Box<Type<TExtDialect>>),

    /// Product type (tuple)
    Product(Vec<Term<TExtDialect>>),
    /// Projection into a term
    Projection(Box<Term<TExtDialect>>, usize),

    /// A case expr, with case arms
    Case(Box<Term<TExtDialect>>, Vec<Arm<TExtDialect>>),

    // let expr with binding, value and then applied context
    Let(
        Box<Pattern<TExtDialect>>,
        Box<Term<TExtDialect>>,
        Box<Term<TExtDialect>>,
    ),
    /// A lambda abstraction
    Abs(Box<Type<TExtDialect>>, Box<Term<TExtDialect>>),
    /// Application of a term to another term
    App(Box<Term<TExtDialect>>, Box<Term<TExtDialect>>),
    /// Type abstraction
    TyAbs(Box<Term<TExtDialect>>),
    /// Type application
    TyApp(Box<Term<TExtDialect>>, Box<Type<TExtDialect>>),

    Fold(Box<Type<TExtDialect>>, Box<Term<TExtDialect>>),
    Unfold(Box<Type<TExtDialect>>, Box<Term<TExtDialect>>),

    /// Introduce an existential type
    /// { *Ty1, Term } as {∃X.Ty}
    /// essentially, concrete representation as interface
    Pack(Box<Type<TExtDialect>>, Box<Term<TExtDialect>>, Box<Type<TExtDialect>>),
    /// Unpack an existential type
    /// open {∃X, bind} in body -- X is bound as a TyVar, and bind as Var(0)
    /// Eliminate an existential type
    Unpack(Box<Term<TExtDialect>>, Box<Term<TExtDialect>>),

    /// Extension
    Extended(TExtDialect::Kind),
}

impl<TExtDialect: SystemRDialect> Default for Kind<TExtDialect> {
    fn default() -> Self {
        Kind::Lit(Literal::Unit)
    }
}

/// Arm of a case expression
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Arm<TExtDialect: SystemRDialect> {
    pub span: Span,
    pub pat: Pattern<TExtDialect>,
    pub term: Box<Term<TExtDialect>>,
}

/// Constant literal expression or pattern
#[derive(Clone, Default, Debug, PartialEq, PartialOrd, Eq, Hash)]
pub enum Literal {
    #[default]
    Unit,
    Bool(bool),
    Nat(u64),
    Tag(String),
    Bytes(Vec<u8>),
}

impl<TExtDialect: SystemRDialect> Term<TExtDialect> {
    pub fn new(kind: Kind<TExtDialect>, span: Span) -> Term<TExtDialect> {
        Term { span, kind }
    }

    #[allow(dead_code)]
    pub const fn unit() -> Term<TExtDialect> {
        Term {
            span: Span::dummy(),
            kind: Kind::Lit(Literal::Unit),
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }

    #[inline]
    pub fn kind(&self) -> &Kind<TExtDialect> {
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
            Literal::Bytes(v) => write!(f, "{:#?}", v),
        }
    }
}

impl<TExtDialect: SystemRDialect> fmt::Display for Term<TExtDialect> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            Kind::Lit(lit) => write!(f, "{}", lit),
            Kind::PlatformBinding(idx) => write!(f, "PlatformBinding({})", idx),
            Kind::Var(v) => write!(f, "#{}", v),
            Kind::Abs(ty, term) => write!(f, "(λ_:{:?}. {})", ty, term),
            Kind::Fix(term) => write!(f, "Fix {:?}", term),
            Kind::Primitive(p) => write!(f, "{:?}", p),
            Kind::Injection(label, tm, ty) => write!(f, "{}({})", label, tm),
            Kind::Projection(term, idx) => write!(f, "{}.{}", term, idx),
            Kind::Product(terms) => write!(
                f,
                "({})",
                terms
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Kind::Case(term, arms) => {
                writeln!(f, "case {} of", term)?;
                for arm in arms {
                    writeln!(f, "\t| {:?} => {},", arm.pat, arm.term)?;
                }
                write!(f, "")
            }
            Kind::Let(pat, t1, t2) => write!(f, "let {:?} = {} in {}", pat, t1, t2),
            Kind::App(t1, t2) => write!(f, "({} {})", t1, t2),
            Kind::TyAbs(term) => write!(f, "(λTy {})", term),
            Kind::TyApp(term, ty) => write!(f, "({} [{:?}])", term, ty),
            Kind::Fold(ty, term) => write!(f, "fold [{:?}] {}", ty, term),
            Kind::Unfold(ty, term) => write!(f, "unfold [{:?}] {}", ty, term),
            Kind::Pack(witness, body, sig) => write!(f, "[|pack {{*{:?}, {}}} as {:?} |]", witness, body, sig),
            Kind::Unpack(m, n) => write!(f, "unpack {} as {}", m, n),
            Kind::Extended(k) => write!(f, "extended (kind: {:?})", k),
        }
    }
}

impl<TExtDialect: SystemRDialect> fmt::Debug for Term<TExtDialect> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

#[cfg(test)]
mod test {
    use crate::dialect::bottom::BottomExtension;

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
