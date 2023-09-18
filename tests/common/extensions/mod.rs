use system_r::{
    extensions::tylet::{TyLetContext, TyLetKind, TyLetParser, TyLetPattern},
    platform_bindings::PlatformBindings,
    syntax::parser::Parser,
    terms::{ExtKind, ExtTerm, Kind, Term},
    types::Context,
};

#[derive(Clone, Default, Debug)]
pub enum OmniContext {
    #[default]
    Empty,
    Bottom(Context),
    TyLet(TyLetContext),
}

#[derive(Clone, Default, Debug)]
pub enum OmniParser<'s> {
    #[default]
    Empty,
    Bottom(Parser<'s>),
    TyLet(TyLetParser<'s>),
}

impl OmniContext {
    pub fn set_platform_bindings(&mut self, pb: PlatformBindings) {
        match self {
            OmniContext::Empty => todo!(),
            OmniContext::Bottom(ctx) => ctx.platform_bindings = pb,
            OmniContext::TyLet(_) => todo!(),
        }
    }
}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub enum OmniKind {
    #[default]
    Empty,
    Bottom(Kind),
    TyLet(ExtKind<TyLetPattern, TyLetKind>),
}
#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub enum OmniTerm {
    #[default]
    Empty,
    Bottom(Term),
    TyLet(ExtTerm<TyLetPattern, TyLetKind>),
}
