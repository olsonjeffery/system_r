use system_r::dialect::bottom::{BottomDialect, BottomExtension, BottomState};
use system_r::patterns::Pattern;
use system_r::platform_bindings::PlatformBindings;
use system_r::terms::visit::{Shift, Subst, TyTermSubst};
use system_r::terms::{Kind, Literal, Primitive, Term};
use system_r::type_check::{Type, TypeChecker};
use system_r::visit::MutTermVisitor;
use anyhow::Result;

pub struct BottomDialectInterpreter {
    platform_bindings: PlatformBindings,
}

impl BottomDialectInterpreter {
    pub fn with_context(context: &TypeChecker<BottomDialect>) -> BottomDialectInterpreter {
        let platform_bindings = context.platform_bindings.clone();
        BottomDialectInterpreter {
            platform_bindings,
        }
    }

    fn normal_form(term: &Term<BottomDialect>) -> bool {
        match &term.kind {
            Kind::Lit(_) => true,
            Kind::Abs(_, _) => true,
            Kind::TyAbs(_) => true,
            Kind::Primitive(_) => true,
            Kind::Injection(_, tm, _) => BottomDialectInterpreter::normal_form(tm),
            Kind::Product(fields) => fields.iter().all(BottomDialectInterpreter::normal_form),
            Kind::Fold(_, tm) => BottomDialectInterpreter::normal_form(tm),
            Kind::Pack(_, tm, _) => BottomDialectInterpreter::normal_form(tm),
            Kind::PlatformBinding(_) => true,
            // Kind::Unpack(pack, tm) => Eval::normal_form(tm),
            _ => false,
        }
    }

    fn eval_primitive(&self, p: Primitive, term: Term<BottomDialect>) -> Result<Option<Term<BottomDialect>>> {
        fn map<F: Fn(u64) -> u64>(f: F, mut term: Term<BottomDialect>) -> Option<Term<BottomDialect>> {
            match &term.kind {
                Kind::Lit(Literal::Nat(n)) => {
                    term.kind = Kind::Lit(Literal::Nat(f(*n)));
                    Some(term)
                }
                _ => None,
            }
        }

        match p {
            Primitive::Succ => Ok(map(|l| l + 1, term)),
            Primitive::Pred => Ok(map(|l| l.saturating_sub(1), term)),
            Primitive::IsZero => match &term.kind {
                Kind::Lit(Literal::Nat(0)) => Ok(Some(Term::new(Kind::Lit(Literal::Bool(true)), term.span))),
                _ => Ok(Some(Term::new(Kind::Lit(Literal::Bool(false)), term.span))),
            },
        }
    }

    pub fn small_step(&self, term: Term<BottomDialect>) -> Result<Option<Term<BottomDialect>>> {
        if BottomDialectInterpreter::normal_form(&term) {
            return Ok(None);
        }
        match term.kind {
            Kind::App(t1, t2) => {
                if BottomDialectInterpreter::normal_form(&t2) {
                    match t1.kind {
                        Kind::Abs(_, mut abs) => {
                            term_subst(*t2, abs.as_mut());
                            Ok(Some(*abs))
                        }
                        Kind::PlatformBinding(idx) => match self.platform_bindings.get(idx) {
                            Some(wc) => {
                                let span = t1.span;
                                match (wc.0)(*t2, &span) {
                                    Ok(t) => Ok(Some(t)),
                                    Err(e) => Err(e),
                                }
                            }
                            _ => Err(anyhow!(
                                "unable to get platform_binding with idx after parsing; shouldn't happen"
                            )),
                        },
                        Kind::Primitive(p) => self.eval_primitive(p, *t2),
                        _ => match self.small_step(*t1)? {
                            Some(t) => Ok(Some(Term::new(Kind::App(Box::new(t), t2), term.span))),
                            None => Ok(None),
                        },
                    }
                } else if BottomDialectInterpreter::normal_form(&t1) {
                    // t1 is in normal form, but t2 is not, so we will
                    // carry out the reducton t2 -> t2', and return
                    // App(t1, t2')
                    let t = self.small_step(*t2)?;
                    match t {
                        Some(t) => Ok(Some(Term::new(Kind::App(t1, Box::new(t)), term.span))),
                        None => Ok(None),
                    }
                } else {
                    // Neither t1 nor t2 are in normal form, we reduce t1 first
                    let t = self.small_step(*t1)?;
                    match t {
                        Some(t) => Ok(Some(Term::new(Kind::App(Box::new(t), t2), term.span))),
                        None => Ok(None),
                    }
                }
            }
            Kind::Let(pat, bind, mut body) => {
                if BottomDialectInterpreter::normal_form(&bind) {
                    // term_subst(*bind, &mut body);
                    BottomDialectInterpreter::case_subst(&pat, &bind, body.as_mut());
                    Ok(Some(*body))
                } else {
                    let t = self.small_step(*bind)?;
                    match t {
                        None => Ok(None),
                        Some(t) => Ok(Some(Term::new(Kind::Let(pat, Box::new(t), body), term.span))),
                    }
                }
            }
            Kind::TyApp(tm, ty) => match tm.kind {
                Kind::TyAbs(mut tm2) => {
                    type_subst(*ty, &mut tm2);
                    Ok(Some(*tm2))
                }
                _ => {
                    let t_prime = self.small_step(*tm)?;
                    match t_prime {
                        Some(t_prime) => Ok(Some(Term::new(Kind::TyApp(Box::new(t_prime), ty), term.span))),
                        None => Ok(None),
                    }
                }
            },
            Kind::Injection(label, tm, ty) => {
                let t_prime = self.small_step(*tm)?;
                match t_prime {
                    Some(t_prime) => Ok(Some(Term::new(
                        Kind::Injection(label, Box::new(t_prime), ty),
                        term.span,
                    ))),
                    None => Ok(None),
                }
            }
            Kind::Projection(tm, idx) => {
                if BottomDialectInterpreter::normal_form(&tm) {
                    match tm.kind {
                        // Typechecker ensures that idx is in bounds
                        Kind::Product(terms) => Ok(terms.get(idx).cloned()),
                        _ => Ok(None),
                    }
                } else {
                    let t_prime = self.small_step(*tm)?;
                    match t_prime {
                        Some(t_prime) => Ok(Some(Term::new(Kind::Projection(Box::new(t_prime), idx), term.span))),
                        None => Ok(None),
                    }
                }
            }
            Kind::Product(terms) => {
                let mut v = Vec::with_capacity(terms.len());
                for term in terms {
                    if BottomDialectInterpreter::normal_form(&term) {
                        v.push(term);
                    } else {
                        let res = self.small_step(term)?;
                        match res {
                            Some(t) => v.push(t),
                            _ => return Ok(None),
                        }
                    }
                }
                Ok(Some(Term::new(Kind::Product(v), term.span)))
            }
            Kind::Fix(tm) => {
                if !BottomDialectInterpreter::normal_form(&tm) {
                    let t_prime = self.small_step(*tm)?;
                    match t_prime {
                        Some(t_prime) => return Ok(Some(Term::new(Kind::Fix(Box::new(t_prime)), term.span))),
                        None => return Ok(None),
                    }
                }

                let x = Term::new(Kind::Fix(tm.clone()), term.span);
                match tm.kind {
                    Kind::Abs(_, mut body) => {
                        term_subst(x, &mut body);
                        Ok(Some(*body))
                    }
                    _ => Ok(None),
                }
            }
            Kind::Case(expr, arms) => {
                if !BottomDialectInterpreter::normal_form(&expr) {
                    let t_prime = self.small_step(*expr)?;
                    match t_prime {
                        Some(t_prime) => return Ok(Some(Term::new(Kind::Case(Box::new(t_prime), arms), term.span))),
                        None => return Ok(None),
                    }
                }

                for mut arm in arms {
                    if arm.pat.matches(&expr, &BottomExtension) {
                        BottomDialectInterpreter::case_subst(&arm.pat, &expr, arm.term.as_mut());
                        return Ok(Some(*arm.term));
                    }
                }

                Ok(None)
            }
            Kind::Fold(ty, tm) => {
                if !BottomDialectInterpreter::normal_form(&tm) {
                    let t_prime = self.small_step(*tm)?;
                    match t_prime {
                        Some(t_prime) => Ok(Some(Term::new(Kind::Fold(ty, Box::new(t_prime)), term.span))),
                        None => Ok(None),
                    }
                } else {
                    Ok(None)
                }
            }

            Kind::Unfold(ty, tm) => {
                if !BottomDialectInterpreter::normal_form(&tm) {
                    let t_prime = self.small_step(*tm)?;
                    match t_prime {
                        Some(t_prime) => return Ok(Some(Term::new(Kind::Unfold(ty, Box::new(t_prime)), term.span))),
                        None => return Ok(None),
                    }
                }

                match tm.kind {
                    // FIXME: unused _ty2 param; incomplete feature/bug?
                    Kind::Fold(_ty2, inner) => Ok(Some(*inner)),
                    _ => Ok(None),
                }
            }
            Kind::Pack(wit, evidence, sig) => {
                if !BottomDialectInterpreter::normal_form(&evidence) {
                    let t_prime = self.small_step(*evidence)?;
                    match t_prime {
                        Some(t_prime) => {
                            return Ok(Some(Term::new(Kind::Pack(wit, Box::new(t_prime), sig), term.span)))
                        }
                        None => return Ok(None),
                    }
                }
                Ok(None)
            }
            Kind::Unpack(package, mut body) => match package.kind {
                // FIXME: unused _sig param; incomplete feature/bug?
                Kind::Pack(wit, evidence, _sig) => {
                    term_subst(*evidence, &mut body);
                    type_subst(*wit, &mut body);
                    Ok(Some(*body))
                }
                _ => {
                    if !BottomDialectInterpreter::normal_form(&package) {
                        let t_prime = self.small_step(*package)?;
                        match t_prime {
                            Some(t_prime) => {
                                return Ok(Some(Term::new(Kind::Unpack(Box::new(t_prime), body), term.span)))
                            }
                            None => return Ok(None),
                        }
                    }
                    Ok(None)
                }
            },

            _ => Ok(None),
        }
    }

    fn case_subst(pat: &Pattern<BottomDialect>, expr: &Term<BottomDialect>, term: &mut Term<BottomDialect>) {
        use Pattern::*;
        match pat {
            Any => {}
            Literal(_) => {}
            Variable(_) => {
                term_subst(expr.clone(), term);
            }
            Product(v) => {
                if let Kind::Product(terms) = &expr.kind {
                    for (idx, tm) in terms.iter().enumerate() {
                        BottomDialectInterpreter::case_subst(&v[idx], tm, term);
                    }
                } else {
                    panic!("runtime error")
                }
            }
            Constructor(label, v) => {
                if let Kind::Injection(label_, tm, _) = &expr.kind {
                    if label == label_ {
                        BottomDialectInterpreter::case_subst(v, tm, term);
                    }
                } else {
                    panic!("runtime error")
                }
            }
            Extended(_) => panic!("extended pattern instructions shouldn't appear here in eval"),
        }
    }
}

fn term_subst(mut s: Term<BottomDialect>, t: &mut Term<BottomDialect>) {
    let mut ext = BottomExtension;
    let state = BottomState;
    Shift::new(1).visit(&mut s, &mut ext, &state).expect("need result here");
    Subst::new(s).visit(t, &mut ext, &state).expect("need result here");
    Shift::new(-1).visit(t, &mut ext, &state).expect("need result here");
}

fn type_subst(s: Type<BottomDialect>, t: &mut Term<BottomDialect>) {
    let mut ext = BottomExtension;
    let state = BottomState;
    let mut tts = match TyTermSubst::new(s, &mut ext, &state) {
        Ok(t) => t,
        Err(e) => panic!("need result here {:?}", e),
    };
    if let Err(e) = tts.visit(t, &mut ext, &state) {
        panic!("need result here {:?}", e)
    }
    if let Err(e) = Shift::new(-1).visit(t, &mut ext, &state) {
        panic!("need result here {:?}", e)
    }
}

#[cfg(test)]
mod test {
    /*
    use crate::interpreter::*;
    use system_r::*;
    use system_r::macros::*;
    use system_r::util::span::Span;

    #[test]
    fn literal() {
        let ctx = type_check::TypeChecker::default();
        let eval = Eval::with_context(&ctx);
        assert_eq!(eval.small_step(lit!(false)).unwrap(), None);
    }

    #[test]
    fn application() {
        let ctx = type_check::TypeChecker::default();
        let eval = Eval::with_context(&ctx);
        let tm = app!(abs!(Type::Nat, app!(prim!(Primitive::Succ), var!(0))), nat!(1));

        let t1 = eval.small_step(tm).unwrap();
        assert_eq!(t1, Some(app!(prim!(Primitive::Succ), nat!(1))));
        let t2 = eval.small_step(t1.unwrap()).unwrap();
        assert_eq!(t2, Some(nat!(2)));
        let t3 = eval.small_step(t2.unwrap()).unwrap();
        assert_eq!(t3, None);
    }

    #[test]
    fn type_application() {
        let ctx = type_check::TypeChecker::default();
        let eval = Eval::with_context(&ctx);
        let tm = tyapp!(
            tyabs!(abs!(Type::Var(0), app!(prim!(Primitive::Succ), var!(0)))),
            Type::Nat
        );

        let t1 = eval.small_step(tm).unwrap();
        assert_eq!(t1, Some(abs!(Type::Nat, app!(prim!(Primitive::Succ), var!(0)))));
        let t2 = eval.small_step(t1.unwrap()).unwrap();
        assert_eq!(t2, None);
    }

    #[test]
    fn projection() {
        let ctx = type_check::TypeChecker::default();
        let eval = Eval::with_context(&ctx);
        let product = Term::new(Kind::Product(vec![nat!(5), nat!(6), nat!(29)]), Span::zero());
        let projection = Term::new(Kind::Projection(Box::new(product), 2), Span::zero());
        let term = app!(prim!(Primitive::Succ), projection);

        let t1 = eval.small_step(term).unwrap();
        assert_eq!(t1, Some(app!(prim!(Primitive::Succ), nat!(29))));
        let t2 = eval.small_step(t1.unwrap()).unwrap();
        assert_eq!(t2, Some(nat!(30)));
        let t3 = eval.small_step(t2.unwrap()).unwrap();
        assert_eq!(t3, None);
    }
    */
}
