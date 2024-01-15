use std::collections::HashMap;

use cucumber::World;

use system_r::{
    dialect::bottom::BottomDialect,
    platform_bindings::PlatformBindings,
    terms::{Kind, Term},
    type_check::Type,
};

use anyhow::Result;

use self::extensions::{OmniKind, OmniState, OmniTerm, OmniType, OmniTypeChecker};

pub mod extensions;
pub mod platform_bindings;

#[derive(Debug, Default, World)]
pub struct SpecsWorld {
    pub code_snippet: String,
    pub type_checkers: HashMap<String, OmniTypeChecker>,
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
    pub last_tyck_success: bool,
    pub last_tyck_type: Type<BottomDialect>,
    pub last_tyck_msg: String,
    pub last_plaintext: String,
}

impl SpecsWorld {
    pub fn take_last_ext_state(&mut self) -> OmniState {
        std::mem::replace(&mut self.last_ext_state, OmniState::Empty)
    }

    pub fn take_ctx_by_name(&mut self, ctx_name: &str) -> Result<OmniTypeChecker> {
        match self.type_checkers.remove(ctx_name) {
            Some(v) => Ok(v),
            None => Err(anyhow!("take_ctx_by_name")),
        }
    }
}
