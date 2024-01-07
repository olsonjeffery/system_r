extern crate cucumber;

use cucumber::{given, then, when};

use system_r::testing;
use system_r::{
    dialect::{SystemRDialect, SystemRExtension, SystemRResolver},
    syntax::parser::Parser,
    terms::Term,
    type_check::{Type, TypeChecker},
};
use system_r_dialects::type_alias::{
    TypeAliasDialectState, TypeAliasExtension, TypeAliasToBottomDialectResolver, TypeAliasTypeChecker,
};

use crate::common::{
    self,
    extensions::{OmniKind, OmniState, OmniTerm, OmniType, OmniTypeChecker},
    SpecsWorld,
};

use super::system_r::{given_a_new_type_checker, BOTTOM_TC_NAME};

use anyhow::Result;

static TYPE_ALIAS_CTX_NAME: &str = "TypeAlias";

pub fn parse_for_extension<TExtDialect: SystemRDialect, TLE: SystemRExtension<TExtDialect>>(
    input: &str,
    ps: &mut Parser<TExtDialect>,
    ext: &mut TLE,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    testing::operate_parser_for(input, ps, ext)
}

pub fn type_check_for_extension<TExtDialect: SystemRDialect + 'static, TLE: SystemRExtension<TExtDialect>>(
    ctx: &mut TypeChecker<TExtDialect>,
    term: &mut Term<TExtDialect>,
    ext: &mut TLE,
) -> Result<Type<TExtDialect>> {
    testing::do_type_check::<TExtDialect, TLE>(ctx, term, ext)
}

pub fn add_type_alias_ext_state_to_context(world: &mut SpecsWorld, st: TypeAliasDialectState) {
    let Some(OmniTypeChecker::TypeAlias(ctx)) = world.type_checkers.get_mut(TYPE_ALIAS_CTX_NAME) else {
        panic!("expected type alias context, didn't get it!");
    };
    ctx.ext_state = st;
}

#[given(regex = r#"^a system_r toolchain extended for TypeAlias"#)]
#[given(regex = r#"^a new TypeAlias context"#)]
fn given_a_new_type_alias_context(world: &mut common::SpecsWorld) {
    world.type_checkers.insert(
        TYPE_ALIAS_CTX_NAME.to_owned(),
        OmniTypeChecker::TypeAlias(TypeAliasTypeChecker::default()),
    );
}

#[then("the last ext should parse successfully")]
fn then_the_last_ext_should_parse_successfully(world: &mut common::SpecsWorld) {
    assert!(world.last_ext_parse_success, "fail msg: {:?}", world.last_ext_parse_msg);
}

#[when("TypeAlias parses the code")]
fn when_it_is_processed_for_type_alias(world: &mut common::SpecsWorld) {
    let input = world.code_snippet.clone();
    let mut ext = TypeAliasExtension;
    let pb = world.platform_bindings.clone();

    let mut ps = Parser::ext_new(&pb, &input, &mut ext);

    let res = parse_for_extension(&input, &mut ps, &mut ext);
    let _term = match res {
        Ok(term) => {
            let k = OmniKind::TypeAlias(term.clone().kind);
            let t = OmniTerm::TypeAlias(term.clone());
            world.last_ext_parse_success = k != OmniKind::Empty;
            world.last_ext_parse_kind = k;
            world.last_ext_parse_term = t;
            world.last_ext_state = OmniState::TypeAlias(ps.to_ext_state());
            term
        }
        Err(e) => {
            panic!("TypeAlias parse failed: {:?}", e);
        }
    };
}

#[when("TypeAlias type checks the code")]
fn when_type_alias_type_checks_the_code(world: &mut SpecsWorld) {
    let OmniTerm::TypeAlias(mut term) = world.last_ext_parse_term.clone() else {
        panic!(
            "expected last ext parse term to be from TypeAliasDialect, wasn't! last ext parse term context: {:?}",
            world.last_ext_parse_term.clone()
        );
    };
    let OmniState::TypeAlias(st) = world.take_last_ext_state() else {
        panic!("expect last ext state to be from TypeAliasDialect, wasn't!");
    };
    add_type_alias_ext_state_to_context(world, st);
    let Some(OmniTypeChecker::TypeAlias(ctx)) = world.type_checkers.get_mut(TYPE_ALIAS_CTX_NAME) else {
        panic!("expected to get a TypeAlias context, didn't!");
    };
    match type_check_for_extension(ctx, &mut term, &mut TypeAliasExtension) {
        Err(e) => panic!("{:?}", e),
        Ok(t) => {
            let ext_state = ctx.ext_state.clone();
            world.last_ext_state = OmniState::TypeAlias(ext_state);
            world.last_ext_ty = OmniType::TypeAlias(t);
        }
    }
}

#[when("TypeAliasDialect is resolved into BottomDialect system_r")]
pub fn when_it_is_converted_to_bottom_dialect(world: &mut SpecsWorld) {
    let tm = match world.last_ext_parse_term.clone() {
        OmniTerm::TypeAlias(t) => t,
        _ => panic!("expected struct data!"),
    };

    let in_ctx = match world.take_ctx_by_name(TYPE_ALIAS_CTX_NAME) {
        Ok(c) => match c {
            OmniTypeChecker::TypeAlias(ctx) => ctx,
            _ => panic!("Expected TypeAlias context, didn't get one!"),
        },
        Err(_) => panic!("expected to get a TypeAlias context, didn't!"),
    };

    let TypeChecker {
        ext_state,
        platform_bindings,
        ..
    } = in_ctx;

    let bottom_tm = {
        // this is an implementation of the SystemRResolver trait
        // for the TypeAliasExtension marker type, it returns a
        // Term<BottomDialect>, something that can be type_check'd
        // and eval'd by the existing system
        match TypeAliasToBottomDialectResolver.resolve(ext_state, tm) {
            Ok(tm) => tm,
            Err(d) => {
                world.last_parse_success = false;
                world.last_parse_msg = d.to_string();
                return;
            }
        }
    };

    world.type_checkers.remove(BOTTOM_TC_NAME);
    given_a_new_type_checker(world);
    world
        .type_checkers
        .get_mut(BOTTOM_TC_NAME)
        .unwrap()
        .set_platform_bindings(platform_bindings);
    world.last_parse_term = bottom_tm.clone();
    world.last_parse_kind = bottom_tm.kind;
    world.last_parse_success = true;
}
