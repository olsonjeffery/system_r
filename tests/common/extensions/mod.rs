use system_r::{
    extensions::struct_data::{StructDataContext, StructDataKind, StructDataPattern, StructDataTokenKind},
    platform_bindings::PlatformBindings,
    terms::{ExtKind, ExtTerm, Kind, Term},
    types::Context, syntax::parser::ParserState, bottom::{BottomTokenKind, BottomKind, BottomPattern},
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
pub enum OmniParser<'s> {
    #[default]
    Empty,
    Bottom(ParserState<'s, BottomTokenKind, BottomKind, BottomPattern>),
    StructData(ParserState<'s, StructDataTokenKind, StructDataKind, StructDataPattern>),
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
    Bottom(Kind),
    StructData(ExtKind<StructDataPattern, StructDataKind>),
}

#[derive(Clone, Default, Debug, PartialEq, PartialOrd)]
pub enum OmniTerm {
    #[default]
    Empty,
    #[allow(dead_code)]
    Bottom(Term),
    StructData(ExtTerm<StructDataPattern, StructDataKind>),
}
