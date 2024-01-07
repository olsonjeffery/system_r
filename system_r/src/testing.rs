/*
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
use crate::dialect::{SystemRDialect, SystemRExtension};

use crate::{
    dialect::bottom::{BottomDialect, BottomExtension},
    eval,
    platform_bindings::Bindings,
    syntax::parser::Parser,
    terms::{visit::InjRewriter, Term},
    type_check::{self, Type},
    visit::MutTermVisitor,
};

use anyhow::Result;

pub fn type_check_term<TExtDialect: SystemRDialect + 'static, TExt: SystemRExtension<TExtDialect>>(
    ctx: &mut type_check::TypeChecker<TExtDialect>,
    term: &mut Term<TExtDialect>,
    ext: &mut TExt,
) -> Result<Type<TExtDialect>> {
    // Step 1
    let ty = ctx.type_check(term, ext)?;
    Ok(ty)
}

pub fn dealias_and_type_check_term<TExtDialect: SystemRDialect + 'static, TExt: SystemRExtension<TExtDialect>>(
    ctx: &mut type_check::TypeChecker<TExtDialect>,
    term: &mut Term<TExtDialect>,
    ext: &mut TExt,
) -> Result<Type<TExtDialect>> {
    // Step 0
    ctx.de_alias(term, ext)?;
    InjRewriter(Default::default(), Default::default()).visit(term, ext, &ctx.ext_state)?;

    type_check_term(ctx, term, ext)
}

pub fn operate_parser_for<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    input: &str,
    ps: &mut Parser<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    ps.parse(ext)
}

pub fn parse_single_block(platform_bindings: &Bindings, input: &str) -> Result<Term<BottomDialect>> {
    let mut ps = Parser::<'_, BottomDialect>::new(platform_bindings, input);
    let mut ext = BottomExtension;
    ps.parse(&mut ext)
}

pub fn do_bottom_eval(
    ctx: &mut type_check::TypeChecker<BottomDialect>,
    term: &mut Term<BottomDialect>,
) -> Result<Term<BottomDialect>> {
    let ev = eval::Eval::with_context(ctx);
    let mut t: Term<BottomDialect> = term.clone();
    let fin = loop {
        if let Some(res) = ev.small_step(t.clone())? {
            t = res;
        } else {
            break t;
        }
    };
    Ok(fin)
}

pub fn type_check_and_eval_single_block(
    ctx: &mut type_check::TypeChecker<BottomDialect>,
    term: &mut Term<BottomDialect>,
    src: &str,
    fail_on_type_mismatch: bool,
) -> Result<(Type<BottomDialect>, Term<BottomDialect>)> {
    // Step 1
    let mut ext = BottomExtension;
    let pre_ty = do_type_check(ctx, term, &mut ext)?;

    // Step 2
    let fin = do_bottom_eval(ctx, term)?;

    // Step 3 -- optional disable?
    let fin_ty = type_check_term(ctx, term, &mut ext)?;
    if fin_ty != pre_ty && fail_on_type_mismatch {
        let msg = format!(
            "Type change of term pre check typecheck to post-eval: {:?} {:?}",
            pre_ty, fin_ty
        );
        return Err(anyhow!("romeo-level type-check-coordination error"));
    }

    Ok((fin_ty, fin))
}

pub fn do_type_check<TExtDialect: SystemRDialect + 'static, TExt: SystemRExtension<TExtDialect>>(
    ctx: &mut type_check::TypeChecker<TExtDialect>,
    term: &mut Term<TExtDialect>,
    ext: &mut TExt,
) -> Result<Type<TExtDialect>> {
    let tc_result = dealias_and_type_check_term(ctx, term, ext)?;
    Ok(tc_result)
}
