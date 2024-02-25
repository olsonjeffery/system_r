use super::BottomDialect;
use crate::{
    dialect::{SystemRDialect, SystemRExtension},
    terms::{plaintext::Plaintext, Kind, Literal, Primitive, Term},
};
use anyhow::Result;

pub fn vanilla_dialect_to_plaintext<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    input: &Term<TExtDialect>,
    ext: &TExt,
) -> Result<String> {
    match input.kind.clone() {
        Kind::Lit(v) => Ok(vanilla_lit_kind_to_plaintext::<TExtDialect>(&v)),
        crate::terms::Kind::Var(_) => todo!("var"),
        crate::terms::Kind::PlatformBinding(_) => todo!("platformbinding"),
        crate::terms::Kind::Fix(_) => todo!("fix"),
        crate::terms::Kind::Primitive(name) => Ok(primitive_to_plaintext(&name)),
        crate::terms::Kind::Injection(_, _, _) => todo!("inj"),
        crate::terms::Kind::Product(_) => todo!("product"),
        crate::terms::Kind::Projection(_, _) => todo!("proj"),
        crate::terms::Kind::Case(_, _) => todo!("case"),
        crate::terms::Kind::Let(_, _, _) => todo!("let"),
        crate::terms::Kind::Abs(_, _) => todo!("abs"),
        crate::terms::Kind::App(l, r) => application_to_plaintext(&l, &r, ext),
        crate::terms::Kind::TyAbs(_) => todo!("tyabs"),
        crate::terms::Kind::TyApp(_, _) => todo!("tyapp"),
        crate::terms::Kind::Fold(_, _) => todo!("fold"),
        crate::terms::Kind::Unfold(_, _) => todo!("unfold"),
        crate::terms::Kind::Pack(_, _, _) => todo!("pack"),
        crate::terms::Kind::Unpack(_, _) => todo!("unpack"),
        k @ Kind::Extended(_) => ext.to_plaintext(input),
    }
}

fn application_to_plaintext<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    lhand: &Box<Term<TExtDialect>>,
    rhand: &Box<Term<TExtDialect>>,
    ext: &TExt) -> Result<String> {
        let lstr = vanilla_dialect_to_plaintext(lhand, ext)?;
        let rstr = vanilla_dialect_to_plaintext(rhand, ext)?;
        Ok(format!("{lstr} {rstr}"))
}

fn primitive_to_plaintext(prim: &Primitive) -> String {
    match prim {
        Primitive::Succ => "succ".to_owned(),
        Primitive::Pred => "pred".to_owned(),
        Primitive::IsZero => "iszero".to_owned(),
    }
}

fn vanilla_lit_kind_to_plaintext<TExtDialect: SystemRDialect>(input: &Literal) -> String {
    match input {
        Literal::Unit => "Unit".to_owned(),
        Literal::Bool(b) => if b == &true { "true".to_owned() } else { "false".to_owned() },
        Literal::Nat(n) => n.to_string(),
        Literal::Tag(t) => format!("{t}"),
        Literal::Bytes(bytes) => {
            let reduced: String = bytes.iter()
                .map(|e| format!("{e}")).collect::<Vec<String>>()
                .join(", ");
            return format!("[{reduced}]");
        },
    }
}

impl Plaintext<BottomDialect> for Term<BottomDialect> {
    fn to_plaintext<TExt: SystemRExtension<BottomDialect>>(&self, ext: &TExt) -> Result<String> {
        match &self.kind {
            // Any dialect building-up from BottomDialect should implement the extended arm for its
            // respective dialect items, otherwise delegate everything else to vanilla_dialect_to_plaintext(),
            // which is exported and should be the extry point for handing it any term
            &Kind::Extended(_) => Err(anyhow!(
                "Plaintext for Term<BottomDialect>: should never have extended
                kinds, but got one (shouldn't ever happen)"
            )),
            _ => vanilla_dialect_to_plaintext(self, ext),
        }
    }
}
