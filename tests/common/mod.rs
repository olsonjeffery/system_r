use std::collections::HashMap;

use cucumber::World;

use system_r::{
    platform_bindings::PlatformBindings,
    terms::{Kind, Term},
    types::Type,
};

use self::extensions::{OmniContext, OmniKind, OmniParser, OmniTerm};

pub mod extensions;
pub mod platform_bindings;

#[derive(Debug, Default, World)]
pub struct SpecsWorld {
    pub code_snippet: String,
    pub contexts: HashMap<String, OmniContext>,
    pub parsers: HashMap<String, OmniParser<'static>>,
    pub platform_bindings: PlatformBindings,
    pub last_parse_success: bool,
    pub last_parse_kind: Kind,
    pub last_parse_term: Term,
    pub last_parse_msg: String,
    pub last_eval_success: bool,
    pub last_eval_fty: Type,
    pub last_eval_kind: Kind,
    pub last_eval_msg: String,
    pub last_eval_term: Term,
    pub last_ext_parse_success: bool,
    pub last_ext_parse_kind: OmniKind,
    pub last_ext_parse_term: OmniTerm,
    pub last_ext_parse_msg: String,
    pub last_ext_ty: Type,
}
