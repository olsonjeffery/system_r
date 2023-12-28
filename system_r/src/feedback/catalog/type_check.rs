use std::collections::VecDeque;

use crate::{
    dialect::SystemRDialect,
    feedback::SystemRFeedback,
    terms::Term,
    type_check::{Type, Variant},
    util::span::Span,
};

/// TC01
pub fn err_01<TExtDialect: SystemRDialect>(span: Span, label: &str) -> SystemRFeedback<TExtDialect> {
    SystemRFeedback::<TExtDialect>::type_check_error(
        "TC01",
        span,
        &format!(r#"constructor {label} doesn't appear in variant fields"#),
    )
}

/// TC02
pub fn err_02<TExtDialect: SystemRDialect>(span: Span, idx: usize) -> SystemRFeedback<TExtDialect> {
    SystemRFeedback::type_check_error("02", span, &format!(r#"type_check: unbound variable {idx}"#))
}

/// TC03
pub fn err_03<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    ty12: Type<TExtDialect>,
    stack: &VecDeque<Type<TExtDialect>>,
) -> SystemRFeedback<TExtDialect> {
    let span = term.span;
    let term_kind = &term.kind;
    SystemRFeedback::type_check_error(
        "TC03",
        span,
        &format!(r#"Type<TExtDialect> mismatch in ext application, ty11 side
            "WTF is ty12? {ty12:?}))
            parent term {term_kind:?}
            ctx stack {stack:?}
            "#),
    )
}

/// TC04
pub fn err_04<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    t1: &Term<TExtDialect>,
    t2: &Term<TExtDialect>,
    ty11: &Type<TExtDialect>,
    ty2: &Type<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    let t1_span = t1.span;
    let t2_span = t2.span;
    let term_span = term.span;
    SystemRFeedback::type_check_error(
        "TC04",
        term_span,
        &format!(r#"Type<TExtDialect> mismatch in application
{t1_span:?} Abstraction requires type {ty11:?}
{t2_span:?} Value has a type of {ty2:?}
    "#),
    )
}

/// TC05
pub fn err_05<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    t1: &Term<TExtDialect>,
    t2: &Term<TExtDialect>,
    ty11: &Type<TExtDialect>,
    ty2: &Type<TExtDialect>,
    stack: &VecDeque<Type<TExtDialect>>,
) -> SystemRFeedback<TExtDialect> {
    let t1_span = t1.span;
    let t2_span = t2.span;
    let term_span = term.span;
    SystemRFeedback::type_check_error(
        "TC05",
        term_span,
        &format!(r#"Type<TExtDialect> mismatch in Binding application
{t1_span:?} Binding Abstraction requires type {ty11:?}
{t2_span:?} Value has a type of {ty2:?}
stack: {stack:?}
    "#),
    )
}

/// TC06
pub fn err_06<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    t1: &TExtDialect::Type,
    stack: &VecDeque<Type<TExtDialect>>,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    SystemRFeedback::type_check_error(
        "TC06",
        term_span,
        &format!(r#"SHOULDNT HAPPEN; type_check on Kind::App value with an extended in t1; term: {term} span: {term_span:?} t: {t1:?} ty+stack {stack:?}"#),
    )
}

/// TC07
pub fn err_07<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    t1: &Term<TExtDialect>,
    ty1: &Type<TExtDialect>,
    ty2: &Type<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    SystemRFeedback::type_check_error(
        "TC07",
        term_span,
        &format!(r#"App: Expected arrow type! Kind::App ty1 {ty1:?}, ty1 {ty2:?} "#),
    )
}

/// TC08
pub fn err_08<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    t1: &Term<TExtDialect>,
    ty1: &Type<TExtDialect>,
    ty2: &Type<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    SystemRFeedback::type_check_error(
        "TC08",
        term_span,
        &format!(r#"Type<TExtDialect> mismatch in fix term Abstraction requires match between {ty1:?} == {ty2:?}"#),
    )
}

/// TC09
pub fn err_09<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    t1: &Term<TExtDialect>,
    ty1: &Type<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    let t1_span = t1.span;
    SystemRFeedback::type_check_error(
        "TC09",
        term_span,
        &format!(r#"Fix: Expected arrow type! {t1_span:?} Kind::fix -> type_check operator has type {ty1:?}"#),
    )
}

/// TC10
pub fn err_10<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    label: &str,
    f: &Variant<TExtDialect>,
    ty_: &Type<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    let f_ty = &f.ty;
    SystemRFeedback::type_check_error(
        "TC10",
        term_span,
        &format!(r#"Invalid associated type in variant -variant {label} requires type {f_ty:?}, but this is {ty_:?} "#),
    )
}

/// TC11
pub fn err_11<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    label: &str,
    fields: String,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    SystemRFeedback::type_check_error(
        "TC11",
        term_span,
        &format!(r#"constructor {label} does not belong to the variant {fields:?}"#),
    )
}

/// TC12
pub fn err_12<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    label: &str,
    ty: &Type<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    SystemRFeedback::type_check_error(
        "TC12",
        term_span,
        &format!(r#"Cannot injection {label} into non-variant type {ty:?}"#),
    )
}

/// TC13
pub fn err_13<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    idx: usize,
    types_len: usize,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    SystemRFeedback::type_check_error(
        "TC13",
        term_span,
        &format!(r#"{idx} is out of range for product of length {types_len}"#),
    )
}

/// TC14
pub fn err_14<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    ty: &Type<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    SystemRFeedback::type_check_error("TC13", term_span, &format!(r#"Cannot project on non-product type {ty:?}"#))
}

/// TC15
pub fn err_15<TExtDialect: SystemRDialect>(term: &Term<TExtDialect>) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    SystemRFeedback::type_check_error("TC15", term_span, &format!(r#"pattern does not match type of binder"#))
}

/// TC16
pub fn err_16<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    ty1: &Type<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    SystemRFeedback::type_check_error("TC16", term_span, &format!(r#"Expected a universal type, not {ty1:?}"#))
}

/// TC17
pub fn err_17<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    rec: &Type<TExtDialect>,
    ty_: &Type<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    SystemRFeedback::type_check_error(
        "TC17",
        term_span,
        &format!(r#"Type<TExtDialect> mismatch in unfold
unfold requires type {rec:?}
term has a type of {ty_:?}
            "#),
    )
}

/// TC18
pub fn err_18<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    rec: &Type<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    SystemRFeedback::type_check_error("TC18", term_span, &format!(r#"execpted a recursive type, not {rec:?}"#))
}

/// TC19
pub fn err_19<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    rec: &Type<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    SystemRFeedback::type_check_error("TC19", term_span, &format!(r#"execpted a recursive type, not {rec:?}"#))
}

/// TC20
pub fn err_20<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    s: &Type<TExtDialect>,
    ty_: &Type<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    SystemRFeedback::type_check_error(
        "TC20",
        term_span,
        &format!(r#"Type<TExtDialect> mismatch in fold
unfold requires type {s:?}
term has a type of {ty_:?}
            "#),
    )
}

/// TC21
pub fn err_21<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    sig_prime: &Type<TExtDialect>,
    evidence: &Term<TExtDialect>,
    evidence_ty: &Type<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    let evidence_span = evidence.span;
    SystemRFeedback::type_check_error(
        "TC21",
        term_span,
        &format!(r#"Type<TExtDialect> mismatch in pack
signature has type {sig_prime:?}
{evidence_span:?} but term has a type {evidence_ty:?}
            "#),
    )
}

/// TC22
pub fn err_22<TExtDialect: SystemRDialect>(
    term: &Term<TExtDialect>,
    signature: &Type<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    let term_span = term.span;
    SystemRFeedback::type_check_error(
        "TC22",
        term_span,
        &format!(r#"Expected an existential type signature, not {signature:?}"#),
    )
}

/// TC23
pub fn err_23<TExtDialect: SystemRDialect>(
    package: &Term<TExtDialect>,
    p_ty: &Type<TExtDialect>,
) -> SystemRFeedback<TExtDialect> {
    let term_span = package.span;
    SystemRFeedback::type_check_error(
        "TC23",
        term_span,
        &format!(r#"Expected an existential type signature, not {p_ty:?}"#),
    )
}

/// TC24
pub fn err_24<TExtDialect: SystemRDialect>(term_span: &Span, idx: usize) -> SystemRFeedback<TExtDialect> {
    SystemRFeedback::type_check_error(
        "TC24",
        *term_span,
        &format!("No matching platform_binding registration for idx {}", idx),
    )
}

/// TC25
pub fn err_25<TExtDialect: SystemRDialect>(term_span: &Span) -> SystemRFeedback<TExtDialect> {
    SystemRFeedback::type_check_error(
        "TC25",
        *term_span,
        "Platform binding with extended args type; not allowed",
    )
}

/// TC26
pub fn err_26<TExtDialect: SystemRDialect>(term_span: &Span) -> SystemRFeedback<TExtDialect> {
    SystemRFeedback::type_check_error("TC26", *term_span, "Binding within binding-type vars; shouldn't happen")
}

/// PATTERN
pub mod pattern {
    use std::collections::HashSet;

    use crate::{
        dialect::SystemRDialect, feedback::SystemRFeedback, patterns::Pattern, terms::Term, type_check::Type,
        util::span::Span,
    };

    /// TCPAT01
    pub fn err_01<TExtDialect: SystemRDialect + 'static>(span: Span) -> SystemRFeedback<TExtDialect> {
        SystemRFeedback::type_check_error("TCPAT01", span, "unreachable pattern!")
    }

    /// TCPAT02
    pub fn err_02<TExtDialect: SystemRDialect>(
        span: Span,
        m_content: Type<TExtDialect>,
        a_pat: Pattern<TExtDialect>,
    ) -> SystemRFeedback<TExtDialect> {
        SystemRFeedback::<TExtDialect>::type_check_error(
            "TCPAT02",
            span,
                &format!(r#"case binding has a type {m_content:?}
                            but this pattern ({a_pat:?}) cannot bind a value of type {m_content:?}."#),
        )
    }

    /// TCPAT03
    pub fn err_03<TExtDialect: SystemRDialect>(
        span: Span,
        set: HashSet<Type<TExtDialect>>,
        expr: &Term<TExtDialect>,
    ) -> SystemRFeedback<TExtDialect> {
        SystemRFeedback::<TExtDialect>::type_check_error(
            "TCPAT03",
            span,
            &format!("incompatible arms! {:?} expr: {:?}", set, expr),
        )
    }

    /// TCPAT04
    pub fn err_04<TExtDialect: SystemRDialect>(span: Span) -> SystemRFeedback<TExtDialect> {
        SystemRFeedback::<TExtDialect>::type_check_error(
            "TCPAT04",
            span,
            "probably unreachable - expected variant type!",
        )
    }

    /// TCPAT05
    pub fn err_05<TExtDialect: SystemRDialect>(span: Span) -> SystemRFeedback<TExtDialect> {
        SystemRFeedback::<TExtDialect>::type_check_error("TCPAT05", span, "patterns are not exhaustive!")
    }
}
