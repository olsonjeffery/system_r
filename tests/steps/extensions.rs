extern crate cucumber;

use core::fmt;
use std::{cell::RefCell, rc::Rc};

use cucumber::{given, then, when};

use system_r::{
    diagnostics::Diagnostic,
    extensions::{
        struct_data::{StructDataContext, StructDataExtension},
        SystemRExtension,
        SystemRConverter
    },
    terms::ExtTerm,
    testing, syntax::parser::{ParserState, self}, bottom::BottomExtension,
};

use crate::common::{
    self,
    extensions::{OmniContext, OmniKind, OmniTerm},
    SpecsWorld,
};

use super::system_r::given_a_new_context;

static StructData_CTX_NAME: &'static str = "StructData";

pub fn parse_for_extension<
    's,
    TExtTokenKind: fmt::Debug + Default + Clone + PartialEq + PartialOrd,
    TExtKind: fmt::Debug + Default + Clone + PartialEq + PartialOrd,
    TEXtPat: fmt::Debug + Default + Clone + PartialEq + PartialOrd,
    TExtState: Clone + Default + fmt::Debug,
    TLE: Default + Copy + Clone + SystemRExtension<TExtTokenKind, TExtKind, TEXtPat, TExtState>,
>(
    input: &str,
    ps: &mut ParserState<'s, TExtTokenKind, TExtKind, TEXtPat, TExtState>,
    ext: &mut TLE,
) -> Result<ExtTerm<TEXtPat, TExtKind>, Diagnostic> {
    testing::operate_parser_for(input, ps, ext)
}

#[given(regex = r#"^a system_r toolchain extended for StructData"#)]
fn given_a_new_StructData_context(world: &mut common::SpecsWorld) {
    world.contexts.insert(
        StructData_CTX_NAME.to_owned(),
        OmniContext::StructData(StructDataContext::default()),
    );
}

#[then("the last ext should parse successfully")]
fn then_the_last_ext_should_parse_successfully(world: &mut common::SpecsWorld) {
    assert!(world.last_ext_parse_success, "fail msg: {:?}", world.last_ext_parse_msg);
}

#[when("it is processed for the StructData extension")]
fn when_it_is_processed_for_StructData(world: &mut common::SpecsWorld) {
    let input = world.code_snippet.clone();
    let mut ext = StructDataExtension;
    let pb = world.platform_bindings.clone();

    let mut ps = parser::ext_new(&pb, &input, &mut ext);

    let res = parse_for_extension(&input, &mut ps, &mut ext);
    let (term, kind) = match res {
        Ok(t) => {
            let k = OmniKind::StructData(t.clone().kind);
            let t = OmniTerm::StructData(t);
            (t, k)
        }
        Err(e) => {
            ps.die();
            world.last_ext_parse_msg = format!("error diag: {:?}", e);
            (OmniTerm::Empty, OmniKind::Empty)
        }
    };

    world.last_ext_parse_success = if kind == OmniKind::Empty { false } else { true };
    world.last_ext_parse_kind = kind;
    world.last_ext_parse_term = term;
}

#[when("StructData-dialect is resolved into bottom-dialect system_r")]
pub fn when_it_is_converted_to_bottom_dialect(world: &mut SpecsWorld) {
    given_a_new_context(world);
    let tm = match world.last_ext_parse_term.clone() {
        OmniTerm::StructData(t) => t,
        _ => panic!("barf!")
    };

    let bottom_tm = match StructDataExtension.resolve(tm) {
        Ok(tm) => tm,
        Err(_) => { world.last_parse_success = false; return; }
    };

    world.last_parse_term = bottom_tm.clone();
    world.last_parse_kind = bottom_tm.kind;
    world.last_parse_success = true;
}