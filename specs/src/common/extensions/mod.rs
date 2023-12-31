use system_r::{
    dialect::bottom::{BottomDialect, BottomKind, BottomState},
    platform_bindings::PlatformBindings,
    terms::{Kind, Term},
    type_check::{Type, TypeChecker},
};
use system_r_dialects::type_alias::{TypeAliasDialect, TypeAliasDialectState, TypeAliasTypeChecker};

#[derive(Clone, Default, Debug)]
pub enum OmniTypeChecker {
    #[default]
    Empty,
    Bottom(TypeChecker<BottomDialect>),
    TypeAlias(TypeAliasTypeChecker),
}

#[allow(dead_code)]
#[derive(Clone, Default, Debug)]
pub enum OmniState {
    #[default]
    Empty,
    Bottom(BottomState),
    TypeAlias(TypeAliasDialectState),
}

impl OmniTypeChecker {
    pub fn set_platform_bindings(&mut self, pb: PlatformBindings) {
        match self {
            OmniTypeChecker::Empty => todo!(),
            OmniTypeChecker::Bottom(ctx) => ctx.platform_bindings = pb,
            OmniTypeChecker::TypeAlias(ctx) => ctx.platform_bindings = pb,
        }
    }
}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub enum OmniKind {
    #[default]
    Empty,
    #[allow(dead_code)]
    Bottom(BottomKind),
    TypeAlias(Kind<TypeAliasDialect>),
}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub enum OmniTerm {
    #[default]
    Empty,
    #[allow(dead_code)]
    Bottom(Term<BottomDialect>),
    TypeAlias(Term<TypeAliasDialect>),
}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd, Eq, Hash)]
pub enum OmniType {
    #[default]
    Empty,
    //Bottom(Type<BottomDialect>),
    TypeAlias(Type<TypeAliasDialect>),
}
