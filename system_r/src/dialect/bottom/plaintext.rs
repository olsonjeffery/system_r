use crate::{terms::{plaintext::Plaintext, Term, Kind}, dialect::{SystemRDialect, SystemRExtension}};
use super::BottomDialect;
use anyhow::Result;

pub fn vanilla_dialect_to_plaintext<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(input: &Term<TExtDialect>, ext: &TExt) -> Result<String> {
    match input.kind.clone() {
        l @ Kind::Lit(_) => Ok(vanilla_lit_kind_to_plaintext(input)),
        crate::terms::Kind::Var(_) => todo!(),
        crate::terms::Kind::PlatformBinding(_) => todo!(),
        crate::terms::Kind::Fix(_) => todo!(),
        crate::terms::Kind::Primitive(_) => todo!(),
        crate::terms::Kind::Injection(_, _, _) => todo!(),
        crate::terms::Kind::Product(_) => todo!(),
        crate::terms::Kind::Projection(_, _) => todo!(),
        crate::terms::Kind::Case(_, _) => todo!(),
        crate::terms::Kind::Let(_, _, _) => todo!(),
        crate::terms::Kind::Abs(_, _) => todo!(),
        crate::terms::Kind::App(_, _) => todo!(),
        crate::terms::Kind::TyAbs(_) => todo!(),
        crate::terms::Kind::TyApp(_, _) => todo!(),
        crate::terms::Kind::Fold(_, _) => todo!(),
        crate::terms::Kind::Unfold(_, _) => todo!(),
        crate::terms::Kind::Pack(_, _, _) => todo!(),
        crate::terms::Kind::Unpack(_, _) => todo!(),
        k @ Kind::Extended(_) => ext.to_plaintext(input),
    }
}

fn vanilla_lit_kind_to_plaintext<TExtDialect: SystemRDialect>(input: &Term<TExtDialect>) -> String {
    todo!()
}

impl Plaintext<BottomDialect> for Term<BottomDialect> {
    fn to_plaintext<TExt: SystemRExtension<BottomDialect>>(&self, ext: &TExt) -> Result<String> {
        match &self.kind {
            // Any dialect building-up from BottomDialect should implement the extended arm for its
            // respective dialect items, otherwise delegate everything else to vanilla_dialect_to_plaintext(),
            // which is exported and should be the extry point for handing it any term
            &Kind::Extended(_) => Err(anyhow!("Plaintext for Term<BottomDialect>: should never have extended
                kinds, but got one (shouldn't ever happen)")),
            _ => vanilla_dialect_to_plaintext(self, ext)
        }
    }
}