use std::collections::HashMap;

use cucumber::World;

use system_r::{
    bottom::BottomDialect,
    platform_bindings::PlatformBindings,
    terms::{Kind, Term},
    types::Type,
};

use self::extensions::{OmniContext, OmniKind, OmniState, OmniTerm, OmniType};

pub mod extensions;
pub mod platform_bindings;

#[derive(Debug, Default, World)]
pub struct SpecsWorld {
    pub code_snippet: String,
    pub contexts: HashMap<String, OmniContext>,
    pub platform_bindings: PlatformBindings,
    pub last_parse_success: bool,
    pub last_parse_kind: Kind<BottomDialect>,
    pub last_parse_term: Term<BottomDialect>,
    pub last_parse_msg: String,
    pub last_eval_success: bool,
    pub last_eval_fty: Type<BottomDialect>,
    pub last_eval_kind: Kind<BottomDialect>,
    pub last_eval_msg: String,
    pub last_eval_term: Term<BottomDialect>,
    pub last_ext_parse_success: bool,
    pub last_ext_parse_kind: OmniKind,
    pub last_ext_parse_term: OmniTerm,
    pub last_ext_parse_msg: String,
    pub last_ext_state: OmniState,
    pub last_ext_ty: OmniType,
}

impl SpecsWorld {
    pub fn take_last_ext_state(&mut self) -> OmniState {
        let st = std::mem::replace(&mut self.last_ext_state, OmniState::Empty);
        st
    }
}
