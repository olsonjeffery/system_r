use super::term::Term;
use super::typing::{Context, Type, TypeError};
use std::rc::Rc;

fn map_var<F: Fn(usize, usize) -> Term>(f: &F, c: usize, t: Rc<Term>) -> Rc<Term> {
    match t.as_ref() {
        Term::True | Term::False => t,
        Term::Abs(ty, t1) => Term::Abs(ty.clone(), map_var(f, c + 1, t1.clone())).into(),
        Term::App(t1, t2) => Term::App(map_var(f, c, t1.clone()), map_var(f, c, t2.clone())).into(),
        Term::If(t1, t2, t3) => Term::If(
            map_var(f, c, t1.clone()),
            map_var(f, c, t2.clone()),
            map_var(f, c, t3.clone()),
        )
        .into(),
        Term::Var(idx) => f(c, *idx).into(),
    }
}

fn shift_above(d: isize, c: usize, t: Rc<Term>) -> Rc<Term> {
    map_var(
        &|c, x| {
            if x >= c {
                Term::Var(x + d as usize)
            } else {
                Term::Var(x)
            }
        },
        c,
        t,
    )
}

fn shift(d: isize, t: Rc<Term>) -> Rc<Term> {
    shift_above(d, 0, t)
}

fn subst(j: usize, s: Rc<Term>, t: Rc<Term>) -> Rc<Term> {
    map_var(
        &|c, x| {
            if x == c {
                Rc::try_unwrap(shift(j as isize, s.clone())).unwrap()
            } else {
                Term::Var(x)
            }
        },
        j,
        t.clone(),
    )
}

pub fn subst_top(s: Rc<Term>, t: Rc<Term>) -> Rc<Term> {
    shift(-1, subst(0, shift(1, s), t))
}

#[derive(Debug)]
pub enum Error {
    NoRuleApplies,
}

fn value(ctx: &Context, term: &Term) -> bool {
    match term {
        Term::True | Term::False | Term::Abs(_, _) => true,
        _ => false,
    }
}

fn eval1(ctx: &Context, term: Rc<Term>) -> Result<Rc<Term>, Error> {
    match term.as_ref() {
        Term::App(t1, ref t2) if value(ctx, &t2) => {
            if let Term::Abs(ty, body) = t1.as_ref() {
                // Err(Error::NoRuleApplies)
                Ok(subst_top(t2.clone(), body.clone()))
            } else {
                Err(Error::NoRuleApplies)
            }
        }
        Term::App(t1, t2) if value(ctx, &t1) => {
            let t_prime = eval1(ctx, t2.clone())?;
            Ok(Term::App(t1.clone(), t_prime).into())
        }
        Term::App(t1, t2) => {
            let t_prime = eval1(ctx, t1.clone())?;
            Ok(Term::App(t_prime, t2.clone()).into())
        }
        Term::If(guard, csq, alt) => match **guard {
            Term::True => Ok(csq.clone()),
            Term::False => Ok(alt.clone()),
            _ => {
                let t_prime = eval1(ctx, guard.clone())?;
                Ok(Term::If(t_prime, csq.clone(), alt.clone()).into())
            }
        },
        _ => Err(Error::NoRuleApplies),
    }
}

pub fn eval(ctx: &Context, term: Rc<Term>) -> Result<Rc<Term>, Error> {
    let mut tp = term;
    loop {
        match eval1(ctx, tp.clone()) {
            Ok(r) => tp = r,
            Err(_) => return Ok(tp),
        }
    }
}
