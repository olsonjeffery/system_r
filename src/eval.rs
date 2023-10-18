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

use crate::bottom::{BottomExtension, BottomKind, BottomPattern, BottomType, BottomDialect};
use crate::diagnostics::Diagnostic;
use crate::patterns::Pattern;
use crate::platform_bindings::PlatformBindings;
use crate::terms::visit::{Shift, Subst, TyTermSubst};
use crate::terms::{ExtKind, ExtTerm, Literal, Primitive, Term};
use crate::types::{Context, Type};
use crate::visit::MutTermVisitor;

pub struct Eval<'ctx> {
    _context: &'ctx Context,
    platform_bindings: PlatformBindings,
}

impl<'ctx> Eval<'ctx> {
    pub fn with_context(_context: &Context) -> Eval<'_> {
        let platform_bindings = _context.platform_bindings.clone();
        Eval {
            _context,
            platform_bindings,
        }
    }

    fn normal_form(term: &ExtTerm<BottomDialect>) -> bool {
        match &term.kind {
            ExtKind::Lit(_) => true,
            ExtKind::Abs(_, _) => true,
            ExtKind::TyAbs(_) => true,
            ExtKind::Primitive(_) => true,
            ExtKind::Injection(_, tm, _) => Eval::normal_form(tm),
            ExtKind::Product(fields) => fields.iter().all(Eval::normal_form),
            ExtKind::Fold(_, tm) => Eval::normal_form(tm),
            ExtKind::Pack(_, tm, _) => Eval::normal_form(tm),
            // Kind::Unpack(pack, tm) => Eval::normal_form(tm),
            _ => false,
        }
    }

    fn eval_primitive(&self, p: Primitive, term: Term) -> Result<Option<Term>, Diagnostic> {
        fn map<F: Fn(u32) -> u32>(f: F, mut term: Term) -> Option<Term> {
            match &term.kind {
                ExtKind::Lit(Literal::Nat(n)) => {
                    term.kind = ExtKind::Lit(Literal::Nat(f(*n)));
                    Some(term)
                }
                _ => None,
            }
        }

        match p {
            Primitive::Succ => Ok(map(|l| l + 1, term)),
            Primitive::Pred => Ok(map(|l| l.saturating_sub(1), term)),
            Primitive::IsZero => match &term.kind {
                ExtKind::Lit(Literal::Nat(0)) => Ok(Some(ExtTerm::new(ExtKind::Lit(Literal::Bool(true)), term.span))),
                _ => Ok(Some(ExtTerm::new(ExtKind::Lit(Literal::Bool(false)), term.span))),
            },
        }
    }

    pub fn small_step(&self, term: Term) -> Result<Option<Term>, Diagnostic> {
        if Eval::normal_form(&term) {
            return Ok(None);
        }
        match term.kind {
            ExtKind::App(t1, t2) => {
                if Eval::normal_form(&t2) {
                    match t1.kind {
                        ExtKind::Abs(_, mut abs) => {
                            term_subst(*t2, abs.as_mut());
                            Ok(Some(*abs))
                        }
                        ExtKind::PlatformBinding(idx) => match self.platform_bindings.get(idx) {
                            Some(wc) => {
                                let span = t1.span;
                                match (wc.0)(*t2, &span) {
                                    Ok(t) => Ok(Some(t)),
                                    Err(_) => Ok(None),
                                }
                            }
                            _ => panic!("unable to get platform_binding with idx after parsing; shouldn't happen"),
                        },
                        ExtKind::Primitive(p) => self.eval_primitive(p, *t2),
                        _ => match self.small_step(*t1)? {
                            Some(t) => Ok(Some(ExtTerm::new(ExtKind::App(Box::new(t), t2), term.span))),
                            None => Ok(None),
                        },
                    }
                } else if Eval::normal_form(&t1) {
                    // t1 is in normal form, but t2 is not, so we will
                    // carry out the reducton t2 -> t2', and return
                    // App(t1, t2')
                    let t = self.small_step(*t2)?;
                    match t {
                        Some(t) => Ok(Some(ExtTerm::new(ExtKind::App(t1, Box::new(t)), term.span))),
                        None => Ok(None),
                    }
                } else {
                    // Neither t1 nor t2 are in normal form, we reduce t1 first
                    let t = self.small_step(*t1)?;
                    match t {
                        Some(t) => Ok(Some(ExtTerm::new(ExtKind::App(Box::new(t), t2), term.span))),
                        None => Ok(None),
                    }
                }
            }
            ExtKind::Let(pat, bind, mut body) => {
                if Eval::normal_form(&bind) {
                    // term_subst(*bind, &mut body);
                    Eval::case_subst(&pat, &bind, body.as_mut());
                    Ok(Some(*body))
                } else {
                    let t = self.small_step(*bind)?;
                    match t {
                        None => Ok(None),
                        Some(t) => Ok(Some(ExtTerm::new(ExtKind::Let(pat, Box::new(t), body), term.span))),
                    }
                }
            }
            ExtKind::TyApp(tm, ty) => match tm.kind {
                ExtKind::TyAbs(mut tm2) => {
                    type_subst(*ty, &mut tm2);
                    Ok(Some(*tm2))
                }
                _ => {
                    let t_prime = self.small_step(*tm)?;
                    match t_prime {
                        Some(t_prime) => Ok(Some(ExtTerm::new(ExtKind::TyApp(Box::new(t_prime), ty), term.span))),
                        None => Ok(None),
                    }
                }
            },
            ExtKind::Injection(label, tm, ty) => {
                let t_prime = self.small_step(*tm)?;
                match t_prime {
                    Some(t_prime) => Ok(Some(ExtTerm::new(
                        ExtKind::Injection(label, Box::new(t_prime), ty),
                        term.span,
                    ))),
                    None => Ok(None),
                }
            }
            ExtKind::Projection(tm, idx) => {
                if Eval::normal_form(&tm) {
                    match tm.kind {
                        // Typechecker ensures that idx is in bounds
                        ExtKind::Product(terms) => Ok(terms.get(idx).cloned()),
                        _ => Ok(None),
                    }
                } else {
                    let t_prime = self.small_step(*tm)?;
                    match t_prime {
                        Some(t_prime) => Ok(Some(ExtTerm::new(
                            ExtKind::Projection(Box::new(t_prime), idx),
                            term.span,
                        ))),
                        None => Ok(None),
                    }
                }
            }
            ExtKind::Product(terms) => {
                let mut v = Vec::with_capacity(terms.len());
                for term in terms {
                    if Eval::normal_form(&term) {
                        v.push(term);
                    } else {
                        let res = self.small_step(term)?;
                        match res {
                            Some(t) => v.push(t),
                            _ => return Ok(None),
                        }
                    }
                }
                Ok(Some(ExtTerm::new(ExtKind::Product(v), term.span)))
            }
            ExtKind::Fix(tm) => {
                if !Eval::normal_form(&tm) {
                    let t_prime = self.small_step(*tm)?;
                    match t_prime {
                        Some(t_prime) => return Ok(Some(ExtTerm::new(ExtKind::Fix(Box::new(t_prime)), term.span))),
                        None => return Ok(None),
                    }
                }

                let x = ExtTerm::new(ExtKind::Fix(tm.clone()), term.span);
                match tm.kind {
                    ExtKind::Abs(_, mut body) => {
                        term_subst(x, &mut body);
                        Ok(Some(*body))
                    }
                    _ => Ok(None),
                }
            }
            ExtKind::Case(expr, arms) => {
                if !Eval::normal_form(&expr) {
                    let t_prime = self.small_step(*expr)?;
                    match t_prime {
                        Some(t_prime) => {
                            return Ok(Some(ExtTerm::new(ExtKind::Case(Box::new(t_prime), arms), term.span)))
                        }
                        None => return Ok(None),
                    }
                }

                for mut arm in arms {
                    if arm.pat.matches(&expr, &BottomExtension) {
                        Eval::case_subst(&arm.pat, &expr, arm.term.as_mut());
                        return Ok(Some(*arm.term));
                    }
                }

                Ok(None)
            }
            ExtKind::Fold(ty, tm) => {
                if !Eval::normal_form(&tm) {
                    let t_prime = self.small_step(*tm)?;
                    match t_prime {
                        Some(t_prime) => Ok(Some(ExtTerm::new(ExtKind::Fold(ty, Box::new(t_prime)), term.span))),
                        None => Ok(None),
                    }
                } else {
                    Ok(None)
                }
            }

            ExtKind::Unfold(ty, tm) => {
                if !Eval::normal_form(&tm) {
                    let t_prime = self.small_step(*tm)?;
                    match t_prime {
                        Some(t_prime) => {
                            return Ok(Some(ExtTerm::new(ExtKind::Unfold(ty, Box::new(t_prime)), term.span)))
                        }
                        None => return Ok(None),
                    }
                }

                match tm.kind {
                    ExtKind::Fold(ty2, inner) => Ok(Some(*inner)),
                    _ => Ok(None),
                }
            }
            ExtKind::Pack(wit, evidence, sig) => {
                if !Eval::normal_form(&evidence) {
                    let t_prime = self.small_step(*evidence)?;
                    match t_prime {
                        Some(t_prime) => {
                            return Ok(Some(ExtTerm::new(
                                ExtKind::Pack(wit, Box::new(t_prime), sig),
                                term.span,
                            )))
                        }
                        None => return Ok(None),
                    }
                }
                Ok(None)
            }
            ExtKind::Unpack(package, mut body) => match package.kind {
                ExtKind::Pack(wit, evidence, sig) => {
                    term_subst(*evidence, &mut body);
                    type_subst(*wit, &mut body);
                    Ok(Some(*body))
                }
                _ => {
                    if !Eval::normal_form(&package) {
                        let t_prime = self.small_step(*package)?;
                        match t_prime {
                            Some(t_prime) => {
                                return Ok(Some(ExtTerm::new(ExtKind::Unpack(Box::new(t_prime), body), term.span)))
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

    fn case_subst(pat: &Pattern<BottomDialect>, expr: &Term, term: &mut Term) {
        use Pattern::*;
        match pat {
            Any => {}
            Literal(_) => {}
            Variable(_) => {
                term_subst(expr.clone(), term);
            }
            Product(v) => {
                if let ExtKind::Product(terms) = &expr.kind {
                    for (idx, tm) in terms.iter().enumerate() {
                        Eval::case_subst(&v[idx], tm, term);
                    }
                } else {
                    panic!("wrong type!")
                }
            }
            Constructor(label, v) => {
                if let ExtKind::Injection(label_, tm, _) = &expr.kind {
                    if label == label_ {
                        Eval::case_subst(v, tm, term);
                    }
                } else {
                    panic!("wrong type!")
                }
            }
            Extended(_) => panic!("extended pattern instructions shouldn't appear here in eval"),
        }
    }
}

fn term_subst(mut s: Term, t: &mut Term) {
    Shift::new(1).visit(&mut s);
    Subst::new(s).visit(t);
    Shift::new(-1).visit(t);
}

fn type_subst(s: Type<BottomType>, t: &mut Term) {
    TyTermSubst::new(s).visit(t);
    Shift::new(-1).visit(t);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::system_r_util::span::Span;

    #[test]
    fn literal() {
        let ctx = crate::types::ExtContext::default();
        let eval = Eval::with_context(&ctx);
        assert_eq!(eval.small_step(lit!(false)).unwrap(), None);
    }

    #[test]
    fn application() {
        let ctx = crate::types::ExtContext::default();
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
        let ctx = crate::types::ExtContext::default();
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
        let ctx = crate::types::ExtContext::default();
        let eval = Eval::with_context(&ctx);
        let product = ExtTerm::new(ExtKind::Product(vec![nat!(5), nat!(6), nat!(29)]), Span::zero());
        let projection = ExtTerm::new(ExtKind::Projection(Box::new(product), 2), Span::zero());
        let term = app!(prim!(Primitive::Succ), projection);

        let t1 = eval.small_step(term).unwrap();
        assert_eq!(t1, Some(app!(prim!(Primitive::Succ), nat!(29))));
        let t2 = eval.small_step(t1.unwrap()).unwrap();
        assert_eq!(t2, Some(nat!(30)));
        let t3 = eval.small_step(t2.unwrap()).unwrap();
        assert_eq!(t3, None);
    }
}
