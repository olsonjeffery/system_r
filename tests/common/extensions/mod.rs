use system_r::{
    bottom::{BottomKind, BottomPattern, BottomState, BottomTokenKind, BottomType},
    extensions::struct_data::{
        StructDataContext, StructDataKind, StructDataPattern, StructDataState, StructDataTokenKind, StructDataType, TypeAliasDialect,
    },
    platform_bindings::PlatformBindings,
    syntax::parser::ParserState,
    terms::{ExtKind, ExtTerm, Term},
    types::{Context, Type},
};

#[derive(Clone, Default, Debug)]
pub enum OmniContext {
    #[default]
    Empty,
    Bottom(Context),
    StructData(StructDataContext),
}

#[allow(dead_code)]
#[derive(Clone, Default, Debug)]
pub enum OmniState {
    #[default]
    Empty,
    Bottom(BottomState),
    StructData(StructDataState),
}

impl OmniContext {
    pub fn set_platform_bindings(&mut self, pb: PlatformBindings) {
        match self {
            OmniContext::Empty => todo!(),
            OmniContext::Bottom(ctx) => ctx.platform_bindings = pb,
            OmniContext::StructData(_) => todo!(),
        }
    }
}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub enum OmniKind {
    #[default]
    Empty,
    #[allow(dead_code)]
    Bottom(BottomKind),
    StructData(ExtKind<TypeAliasDialect>),
}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub enum OmniTerm {
    #[default]
    Empty,
    #[allow(dead_code)]
    Bottom(Term),
    StructData(ExtTerm<TypeAliasDialect>),
}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd, Eq, Hash)]
pub enum OmniType {
    #[default]
    Empty,
    Bottom(Type<BottomType>),
    StructData(Type<StructDataType>),
}
