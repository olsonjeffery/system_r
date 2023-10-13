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
use crate::syntax::parser::ParserState;
use crate::system_r_util::span::Span;
use crate::terms::Term;
use core::fmt;
use std::hash;

use crate::bottom::{BottomState, BottomTokenKind, BottomType};
use crate::extensions::SystemRExtension;

use crate::{
    bottom::{BottomExtension, BottomKind, BottomPattern},
    diagnostics::Diagnostic,
    eval,
    platform_bindings::PlatformBindings,
    syntax::{error::Error, parser, parser::ErrorKind},
    terms::{visit::InjRewriter, ExtTerm},
    types::{self, Type},
    visit::MutTermVisitor,
};

pub fn code_format(src: &str, diag: Diagnostic) -> String {
    let srcl = src.lines().collect::<Vec<&str>>();

    let mut msgs = diag.other.clone();
    msgs.insert(0, diag.primary.clone());

    let mut output = "".to_string();

    for line in diag.lines() {
        for anno in &msgs {
            if anno.span.start.line != line {
                continue;
            }
            let empty = (0..anno.span.start.col + 3).map(|_| ' ').collect::<String>();
            let tilde = (1..anno.span.end.col.saturating_sub(anno.span.start.col))
                .map(|_| '~')
                .collect::<String>();
            output += "\r\n";
            output += &format!("{}^{}^ --- {}", empty, tilde, anno.info);
        }
    }

    output
}

pub fn type_check_term<
    's,
    TExtTokenKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExtType: Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: Clone + fmt::Debug + Default,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ctx: &mut types::ExtContext<TExtTokenKind, TExtKind, TExtPat, TExtType>,
    term: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
    ext: &mut TExt,
) -> Result<Type<TExtType>, Diagnostic> {
    // Step 1
    let ty = ctx.type_check(term, ext)?;
    Ok(ty)
}

pub fn dealias_and_type_check_term<
    's,
    TExtTokenKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExtType: Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: fmt::Debug + Clone + Default,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ctx: &mut types::ExtContext<TExtTokenKind, TExtKind, TExtPat, TExtType>,
    term: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
    ext: &mut TExt,
) -> Result<Type<TExtType>, Diagnostic> {
    // Step 0
    ctx.de_alias(term);
    InjRewriter(Default::default(), Default::default()).visit(term);

    type_check_term(ctx, term, ext)
}

pub fn operate_parser_for<
    's,
    TExtTokenKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TEXtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExtType: Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: Clone + fmt::Debug + Default,
    TExt: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TEXtPat, TExtType, TExtState>,
>(
    input: &str,
    ps: &mut ParserState<'s, TExtTokenKind, TExtKind, TEXtPat, TExtType, TExtState>,
    ext: &mut TExt,
) -> Result<ExtTerm<TEXtPat, TExtKind, TExtType>, Diagnostic> {
    return match parser::parse(ps, ext) {
        Ok(term) => Ok(term),
        Err(Error {
            kind: ErrorKind::Eof, ..
        }) => return Err(Diagnostic::error(Span::default(), "Unexpected EOF")),
        Err(e) => {
            //dbg!(e);
            let diagnostic_message = parser::diagnostic(ps.clone()).emit();
            let msg = format!("operate ERROR {:?}\r\nDIAGNOSTIC: {:?}", e.clone(), diagnostic_message);
            return Err(Diagnostic::error(Span::default(), msg));
        }
    };
}

pub fn parse_single_block(
    platform_bindings: &PlatformBindings,
    input: &str,
) -> Result<ExtTerm<BottomPattern, BottomKind, BottomType>, Diagnostic> {
    let mut ps = parser::new::<BottomType>(platform_bindings, input);
    let mut ext = BottomExtension;
    return match parser::parse(&mut ps, &mut ext) {
        Ok(term) => Ok(term),
        Err(Error {
            kind: ErrorKind::Eof, ..
        }) => return Err(Diagnostic::error(Span::default(), "Unexpected EOF")),
        Err(e) => {
            //dbg!(e);
            let diagnostic_message = parser::diagnostic(ps).emit();
            let msg = format!(
                "bottom parse ERROR {:?}\r\nDIAGNOSTIC: {:?}",
                e.clone(),
                diagnostic_message
            );
            return Err(Diagnostic::error(Span::default(), msg));
        }
    };
}

pub fn do_bottom_eval(
    ctx: &mut types::ExtContext<BottomTokenKind, BottomKind, BottomPattern, BottomType>,
    term: &mut ExtTerm<BottomPattern, BottomKind, BottomType>,
) -> Result<ExtTerm<BottomPattern, BottomKind, BottomType>, Diagnostic> {
    let ev = eval::Eval::with_context(ctx);
    let mut t: Term = term.clone();
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
    ctx: &mut types::ExtContext<BottomTokenKind, BottomKind, BottomPattern, BottomType>,
    term: &mut ExtTerm<BottomPattern, BottomKind, BottomType>,
    src: &str,
    fail_on_type_mismatch: bool,
) -> Result<(Type<BottomType>, ExtTerm<BottomPattern, BottomKind, BottomType>), Diagnostic> {
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
        return Err(Diagnostic::error(fin.span(), msg));
    }

    Ok((fin_ty, fin))
}

pub fn do_type_check<
    's,
    TExtTokenKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExtType: Clone + fmt::Debug + Default + PartialEq + PartialOrd + Eq + hash::Hash,
    TExtState: Clone + fmt::Debug + Default,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TExtPat, TExtType, TExtState>,
>(
    ctx: &mut types::ExtContext<TExtTokenKind, TExtKind, TExtPat, TExtType>,
    term: &mut ExtTerm<TExtPat, TExtKind, TExtType>,
    ext: &mut TExt,
) -> Result<Type<TExtType>, Diagnostic> {
    let tc_result = dealias_and_type_check_term(ctx, term, ext);
    let Some(pre_ty) = tc_result.clone().ok() else {
        let e = tc_result.err().unwrap();
        return Err(e);
    };
    Ok(pre_ty)
}
