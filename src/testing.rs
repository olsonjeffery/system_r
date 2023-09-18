use core::fmt;

use crate::bottom::BottomTokenKind;
use crate::extensions::SystemRExtension;
use crate::syntax::parser::ExtParser;
/*
Copyright (C) 2023 AUTHORS

GNU Lesser General Public License Version 3

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
use crate::system_r_util::span::Span;

use crate::{
    bottom::{BottomExtension, BottomKind, BottomPattern},
    diagnostics::Diagnostic,
    eval,
    platform_bindings::PlatformBindings,
    syntax::parser,
    syntax::parser::Parser,
    terms::{visit::InjRewriter, ExtTerm, Term},
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

    return output;
}

pub fn type_check_term(
    ctx: &mut types::ExtContext<BottomTokenKind, BottomKind, BottomPattern, BottomExtension>,
    term: &mut ExtTerm<BottomPattern, BottomKind>,
) -> Result<Type, Diagnostic> {
    // Step 1
    let ty = ctx.type_check(&term)?;
    Ok(ty)
}

pub fn dealias_and_type_check_term(
    ctx: &mut types::ExtContext<BottomTokenKind, BottomKind, BottomPattern, BottomExtension>,
    term: &mut ExtTerm<BottomPattern, BottomKind>,
) -> Result<Type, Diagnostic> {
    // Step 0
    ctx.de_alias(term);
    InjRewriter(BottomPattern::Placeholder, BottomKind::Placeholder).visit(term);

    type_check_term(ctx, term)
}

pub fn operate_parser_for<
    's,
    TExtTokenKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExtKind: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TEXtPat: Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TLE: Clone + SystemRExtension<TExtTokenKind, TExtKind, TEXtPat>,
>(
    mut parser: ExtParser<'s, TExtTokenKind, TExtKind, TEXtPat, TLE>,
    input: &str,
) -> Result<ExtTerm<TEXtPat, TExtKind>, Diagnostic> {
    return match parser.parse() {
        Ok(term) => Ok(term),
        Err(parser::Error {
            kind: parser::ErrorKind::Eof,
            ..
        }) => return Err(Diagnostic::error(Span::default(), "Unexpected EOF")),
        Err(e) => {
            //dbg!(e);
            let diagnostic_message = parser.diagnostic().emit();
            let msg = format!("ERROR {:?}\r\nDIAGNOSTIC: {:?}", e.clone(), diagnostic_message);
            return Err(Diagnostic::error(Span::default(), msg));
        }
    };
}

pub fn parse_single_block(
    platform_bindings: &PlatformBindings,
    input: &str,
) -> Result<ExtTerm<BottomPattern, BottomKind>, Diagnostic> {
    let mut p = Parser::new(platform_bindings, input);
    return match p.parse() {
        Ok(term) => Ok(term),
        Err(parser::Error {
            kind: parser::ErrorKind::Eof,
            ..
        }) => return Err(Diagnostic::error(Span::default(), "Unexpected EOF")),
        Err(e) => {
            //dbg!(e);
            let diagnostic_message = p.diagnostic().emit();
            let msg = format!("ERROR {:?}\r\nDIAGNOSTIC: {:?}", e.clone(), diagnostic_message);
            return Err(Diagnostic::error(Span::default(), msg));
        }
    };
}

pub fn type_check_and_eval_single_block(
    ctx: &mut types::ExtContext<BottomTokenKind, BottomKind, BottomPattern, BottomExtension>,
    term: &mut ExtTerm<BottomPattern, BottomKind>,
    src: &String,
    fail_on_type_mismatch: bool,
) -> Result<(Type, ExtTerm<BottomPattern, BottomKind>), Diagnostic> {
    // Step 0-1
    let tc_result = dealias_and_type_check_term(ctx, term);
    let Some(pre_ty) = tc_result.clone().ok() else {
        let e = tc_result.err().unwrap();
        return Err(e);
    };

    // Step 2
    let ev = eval::Eval::with_context(ctx);
    let mut t: Term = term.clone();
    let fin = loop {
        if let Some(res) = ev.small_step(t.clone())? {
            t = res;
        } else {
            break t;
        }
    };

    // Step 3 -- optional disable?
    let fin_ty = type_check_term(ctx, term)?;
    if fin_ty != pre_ty && fail_on_type_mismatch {
        let msg = format!(
            "Type change of term pre check typecheck to post-eval: {:?} {:?}",
            pre_ty, fin_ty
        );
        return Err(Diagnostic::error(fin.span(), msg));
    }

    Ok((fin_ty, fin))
}
