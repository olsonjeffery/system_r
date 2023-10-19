use system_r::bottom::{BottomDialect, BottomKind, BottomPattern, BottomType};
use system_r::system_r_util::span::Span;
use system_r::terms::{Kind, Term};
use system_r::{diagnostics::Diagnostic, platform_bindings::WrappedContent, terms::Literal, types::Type};

pub fn pull_u32_from(args: &Vec<Term<BottomDialect>>, idx: usize, span: &Span) -> Result<u32, Diagnostic> {
    let arg_t_raw = match args.get(idx) {
        Some(t) => t,
        None => {
            return Err(Diagnostic::error(
                *span,
                format!("pull_u32_from: nothing in arg slot for idx {}", idx),
            ))
        }
    };
    let arg_actual = match arg_t_raw.kind.clone() {
        Kind::Lit(Literal::Nat(n)) => n,
        k => {
            return Err(Diagnostic::error(
                *span,
                format!("pull_u32_from: expected a Nat lit kind, got {:?}", k),
            ))
        }
    };
    Ok(arg_actual)
}

pub fn pb_add() -> WrappedContent {
    WrappedContent(add, Type::Product(vec![Type::Nat, Type::Nat]), Type::Nat)
}
fn add(arg: Term<BottomDialect>, span: &Span) -> Result<Term<BottomDialect>, Diagnostic> {
    match arg.kind {
        Kind::Product(args) => {
            if args.len() != 2 {
                return Err(Diagnostic::error(*span, "nat::arith::add: expected product of len 2"));
            }
            let arg0_actual = pull_u32_from(&args, 0, span)?;
            let arg1_actual = pull_u32_from(&args, 1, span)?;
            let sum = Kind::Lit(Literal::Nat(arg0_actual + arg1_actual));
            let mut ret_term = Term::unit();
            ret_term.kind = sum.clone();
            ret_term.span = *span;
            return Ok(ret_term);
        }
        _ => {
            return Err(Diagnostic::error(
                arg.span(),
                format!("nat::arith::add: Expected a product argument"),
            ))
        }
    };
}
