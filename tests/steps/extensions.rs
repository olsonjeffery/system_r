extern crate cucumber;

use core::fmt;
use std::{cell::RefCell, hash, rc::Rc};

use cucumber::{given, then, when};

use system_r::{
    bottom::BottomExtension,
    diagnostics::Diagnostic,
    extensions::{
        struct_data::{StructDataContext, StructDataExtension, StructDataState, TypeAliasDialect},
        SystemRDialect, SystemRExtension, SystemRTranslator,
    },
    syntax::parser::{self, ParserState},
    terms::Term,
    testing::{self, code_format, do_type_check},
    types::{Context, Type},
};

use crate::common::{
    self,
    extensions::{OmniContext, OmniKind, OmniState, OmniTerm, OmniType},
    SpecsWorld,
};

use super::system_r::given_a_new_context;

static StructData_CTX_NAME: &'static str = "StructData";

pub fn parse_for_extension<
    's,
    TExtDialect: SystemRDialect + fmt::Debug + Default + Clone + PartialEq + PartialOrd,
    TLE: Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    input: &str,
    ps: &mut ParserState<'s, TExtDialect>,
    ext: &mut TLE,
) -> Result<Term<TExtDialect>, Diagnostic> {
    testing::operate_parser_for(input, ps, ext)
}

pub fn type_check_for_extension<
    's,
    TExtDialect: SystemRDialect + fmt::Debug + Default + Clone + PartialEq + PartialOrd + Eq,
    TLE: Default + Copy + Clone + SystemRExtension<TExtDialect> + fmt::Debug,
>(
    ctx: &mut Context<TExtDialect>,
    term: &mut Term<TExtDialect>,
    ext: &mut TLE,
) -> Result<Type<TExtDialect::TExtType>, Diagnostic> {
    testing::do_type_check::<TExtDialect, TLE>(ctx, term, ext)
}

pub fn add_type_alias_ext_state_to_context<'s>(world: &mut SpecsWorld, st: StructDataState) {
    let Some(OmniContext::StructData(ctx)) = world.contexts.get_mut(StructData_CTX_NAME) else {
        panic!("expected type alias context, didn't get it!");
    };
    ctx.ext_state = st;
}

#[given(regex = r#"^a system_r toolchain extended for TypeAlias"#)]
#[given(regex = r#"^a new TypeAlias context"#)]
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

#[when("TypeAlias parses the code")]
fn when_it_is_processed_for_StructData(world: &mut common::SpecsWorld) {
    let input = world.code_snippet.clone();
    let mut ext = StructDataExtension;
    let pb = world.platform_bindings.clone();

    let mut ps = parser::ext_new(&pb, &input, &mut ext);

    let res = parse_for_extension(&input, &mut ps, &mut ext);
    let term = match res {
        Ok(term) => {
            let k = OmniKind::StructData(term.clone().kind);
            let t = OmniTerm::StructData(term.clone());
            world.last_ext_parse_success = if k == OmniKind::Empty { false } else { true };
            world.last_ext_parse_kind = k;
            world.last_ext_parse_term = t;
            world.last_ext_state = OmniState::StructData(ps.to_ext_state());
            term
        }
        Err(e) => {
            ps.die();
            world.last_ext_parse_msg = format!("error diag: {:?}", e);
            return;
        }
    };
}

#[when("TypeAlias type checks the code")]
fn when_type_alias_type_checks_the_code(world: &mut SpecsWorld) {
    let OmniTerm::StructData(mut term) = world.last_ext_parse_term.clone() else {
        panic!("expected last ext parse term to be from TypeAliasDialect, wasn't!");
    };
    let OmniState::StructData(st) = world.take_last_ext_state() else {
        panic!("expect last ext state to be from TypeAliasDialect, wasn't!");
    };
    add_type_alias_ext_state_to_context(world, st);
    let Some(OmniContext::StructData(ctx)) = world.contexts.get_mut(StructData_CTX_NAME) else {
        panic!("expected to get a TypeAlias context, didn't!");
    };
    match type_check_for_extension(ctx, &mut term, &mut StructDataExtension) {
        Err(e) => panic!("{:?}", e),
        _ => {}
    }
}

#[when("TypeAlias-dialect is resolved into bottom-dialect system_r")]
pub fn when_it_is_converted_to_bottom_dialect(world: &mut SpecsWorld) {
    let tm = match world.last_ext_parse_term.clone() {
        OmniTerm::StructData(t) => t,
        _ => panic!("expected struct data!"),
    };

    let mut ps = match &world.last_ext_state {
        OmniState::StructData(ps) => ps.clone(),
        _ => panic!("expected OmniState::StructData"),
    };

    let bottom_tm = match StructDataExtension.resolve(&mut ps, tm) {
        Ok(tm) => tm,
        Err(d) => {
            world.last_parse_success = false;
            world.last_parse_msg = code_format("", d);
            return;
        }
    };

    world.last_parse_term = bottom_tm.clone();
    world.last_parse_kind = bottom_tm.kind;
    world.last_parse_success = true;
}
