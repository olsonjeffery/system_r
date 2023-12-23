use anyhow::Result;
use system_r::bottom::BottomDialect;
use system_r::feedback::type_check::TypeCheckerDiagnosticInfo;
use system_r::util::span::Span;
use system_r::terms::{Kind, Term};
use system_r::{platform_bindings::WrappedBinding, terms::Literal, type_check::Type};

pub fn pull_u64_from(args: &Vec<Term<BottomDialect>>, idx: usize, span: &Span) -> Result<u64> {
    let arg_t_raw = match args.get(idx) {
        Some(t) => t,
        None => {
            return Err(TypeCheckerDiagnosticInfo::error(*span, format!("pull_u32_from: nothing in arg slot for idx {}", idx)).into())
        }
    };
    let arg_actual = match arg_t_raw.kind.clone() {
        Kind::Lit(Literal::Nat(n)) => n,
        k => {
            return Err(TypeCheckerDiagnosticInfo::error(*span, format!("pull_u32_from: expected a Nat lit kind, got {:?}", k)).into())
        }
    };
    Ok(arg_actual)
}

pub fn pb_sub() -> WrappedBinding {
    WrappedBinding(sub, Type::Product(vec![Type::Nat, Type::Nat]), Type::Nat)
}
pub fn pb_add() -> WrappedBinding {
    WrappedBinding(add, Type::Product(vec![Type::Nat, Type::Nat]), Type::Nat)
}
fn sub(arg: Term<BottomDialect>, span: &Span) -> Result<Term<BottomDialect>> {
    match arg.kind {
        Kind::Product(args) => {
            if args.len() != 2 {
                return Err(TypeCheckerDiagnosticInfo::error(*span, "nat::arith::sub: expected product of len 2").into());
            }
            let arg0_actual = pull_u64_from(&args, 0, span)?;
            let arg1_actual = pull_u64_from(&args, 1, span)?;
            let sum = Kind::Lit(Literal::Nat(arg0_actual - arg1_actual));
            let mut ret_term = Term::unit();
            ret_term.kind = sum.clone();
            ret_term.span = *span;
            return Ok(ret_term);
        }
        _ => return Err(TypeCheckerDiagnosticInfo::error(arg.span(), format!("nat::arith::sub: Expected a product argument")).into()),
    };
}
fn add(arg: Term<BottomDialect>, span: &Span) -> Result<Term<BottomDialect>> {
    match arg.kind {
        Kind::Product(args) => {
            if args.len() != 2 {
                return Err(TypeCheckerDiagnosticInfo::error(*span, "nat::arith::add: expected product of len 2").into());
            }
            let arg0_actual = pull_u64_from(&args, 0, span)?;
            let arg1_actual = pull_u64_from(&args, 1, span)?;
            let sum = Kind::Lit(Literal::Nat(arg0_actual + arg1_actual));
            let mut ret_term = Term::unit();
            ret_term.kind = sum.clone();
            ret_term.span = *span;
            return Ok(ret_term);
        }
        _ => return Err(TypeCheckerDiagnosticInfo::error(arg.span(), format!("nat::arith::add: Expected a product argument")).into()),
    };
}
