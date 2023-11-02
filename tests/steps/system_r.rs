extern crate cucumber;

use cucumber::{gherkin::Step, given, then, when};

use system_r::{
    terms::{Kind, Literal, Term},
    testing::{self, code_format, do_bottom_eval},
    types::Context,
    types::Type,
};

use crate::common::{self, extensions::OmniContext};

pub static BOTTOM_CTX_NAME: &'static str = "system_r_bottom";

#[given(regex = r#"^a new ctx"#)]
#[given(regex = r#"^a new sr ctx"#)]
#[given(regex = r#"^a new system_r context"#)]
pub fn given_a_new_context(world: &mut common::SpecsWorld) {
    let new_ctx = Context::default();
    world
        .contexts
        .insert(BOTTOM_CTX_NAME.to_string(), OmniContext::Bottom(new_ctx));
}

#[given(regex = r#"^a code block:(\w?.*)$"#)]
#[given(regex = r#"^a srpt block:(\w?.*)$"#)]
#[given(regex = r#"^a system_r plain text block:(\w?.*)$"#)]
fn given_a_code_snippet(world: &mut common::SpecsWorld, step: &Step) {
    world.code_snippet = step.docstring.clone().unwrap();
}

#[given(regex = r#"an instrinsic for Nat addition named iiiNatAdd"#)]
fn given_an_intrinsic_for_nat_addition_named_i_nat_add(world: &mut common::SpecsWorld) {
    let pb = &mut world.platform_bindings;

    pb.register("iiiNatAdd", common::platform_bindings::arith::pb_add());

    world
        .contexts
        .get_mut(BOTTOM_CTX_NAME)
        .unwrap()
        .set_platform_bindings(pb.clone());
}

#[when("it evals successfully")]
#[when("it runs")]
#[when("sr evals it successfully")]
#[when("sr runs it")]
fn when_it_evals_successfully(world: &mut common::SpecsWorld) {
    when_it_is_parsed_and_evaluated(world);
    then_the_last_parse_should_be_successful(world);
    then_the_last_eval_should_be_successful(world);
}
#[when("it is parsed")]
#[when("sr parses it")]
fn when_it_is_parsed(world: &mut common::SpecsWorld) {
    if world.code_snippet == "" {
        assert!(false, "passed empty code snippet to parser");
    }
    let parse_result = testing::parse_single_block(&world.platform_bindings, &world.code_snippet);
    let eval_res = match parse_result {
        Ok(term) => term,
        Err(e) => {
            world.last_parse_success = false;
            world.last_parse_kind = Kind::Lit(Literal::Unit);
            world.last_parse_msg = code_format(&world.code_snippet, e);
            world.last_parse_term = Term::unit();
            return;
        }
    };
    world.last_parse_success = true;
    world.last_parse_term = eval_res.clone();
    world.last_parse_kind = eval_res.kind;
    world.last_parse_msg = "".to_owned();
}
#[when("bottom eval is ran")]
fn when_bottom_eval_is_ran(world: &mut common::SpecsWorld) {
    let Some(OmniContext::Bottom(ctx)) = world.contexts.get_mut(BOTTOM_CTX_NAME) else {
        panic!("expected to get a bottom context, didn't!");
    };
    let mut term = world.last_parse_term.clone();

    match do_bottom_eval(ctx, &mut term) {
        Ok(t) => {
            world.last_eval_success = true;
            world.last_eval_term = t.clone();
            world.last_eval_fty = Type::Unit;
            world.last_eval_kind = t.kind;
            world.last_eval_msg = "".to_owned();
        }
        Err(e) => {
            world.last_eval_success = false;
            world.last_eval_term = Term::unit();
            world.last_eval_fty = Type::Unit;
            world.last_eval_kind = Kind::Lit(Literal::Unit);
            world.last_eval_msg = code_format(&world.code_snippet, e);
            return;
        }
    }
}

#[when("eval is ran")]
#[when("sr evals it")]
fn when_eval_is_ran(world: &mut common::SpecsWorld) {
    if world.code_snippet == "" {
        assert!(false, "passed empty code snippet to parser");
    }
    let Some(OmniContext::Bottom(ctx)) = world.contexts.get_mut(BOTTOM_CTX_NAME) else {
        panic!("expected to get a bottom context, didn't!");
    };
    let mut term = world.last_parse_term.clone();

    let eval_res = testing::type_check_and_eval_single_block(ctx, &mut term, &world.code_snippet, false);
    let eval_pair = match eval_res {
        Ok(t) => t,
        Err(e) => {
            world.last_eval_success = false;
            world.last_eval_term = Term::unit();
            world.last_eval_fty = Type::Unit;
            world.last_eval_kind = Kind::Lit(Literal::Unit);
            world.last_eval_msg = code_format(&world.code_snippet, e);
            return;
        }
    };
    let (eval_ty, eval_res) = eval_pair;
    world.last_eval_success = true;
    world.last_eval_term = eval_res.clone();
    world.last_eval_fty = eval_ty;
    world.last_eval_kind = eval_res.kind;
    world.last_eval_msg = "".to_owned();
}

#[when("it is parsed and evaluated")]
#[when("sr parses and evals it")]
fn when_it_is_parsed_and_evaluated(world: &mut common::SpecsWorld) {
    when_it_is_parsed(world);
    when_eval_is_ran(world);
}

#[then("the last eval should be successful")]
#[then("the last sr eval should be successful")]
fn then_the_last_eval_should_be_successful(world: &mut common::SpecsWorld) {
    assert!(world.last_eval_success == true, "{}", world.last_eval_msg);
}

#[then("the last parse should be successful")]
#[then("the last sr parse should be successful")]
fn then_the_last_parse_should_be_successful(world: &mut common::SpecsWorld) {
    assert!(world.last_parse_success == true, "{}", world.last_parse_msg);
}

#[then("the last eval should have failed")]
#[then("the last sr eval should have failed")]
fn then_the_last_eval_should_have_failed(world: &mut common::SpecsWorld) {
    assert!(world.last_eval_success == false);
}

#[then("the last parse should have failed")]
#[then("the last sr parse should have failed")]
fn then_the_last_parse_should_have_failed(world: &mut common::SpecsWorld) {
    assert!(world.last_parse_success == false);
}

#[then("the resulting eval Kind should be Unit")]
#[then("the resulting sr eval Kind should be Unit")]
fn then_the_evaluated_term_should_be_unit(world: &mut common::SpecsWorld) {
    assert!(
        world.last_eval_kind == Kind::Lit(Literal::Unit),
        "Kind {:?}",
        world.last_eval_kind
    );
}

#[then("the resulting eval Kind should be Boolean false")]
#[then("the resulting sr eval Kind should be Boolean false")]
fn then_the_evaluated_term_should_be_boolean_false(world: &mut common::SpecsWorld) {
    assert!(
        world.last_eval_kind == Kind::Lit(Literal::Bool(false)),
        "Expected Boolean false, got {:?}",
        world.last_eval_kind
    );
}

#[then("the resulting eval Kind should be Boolean true")]
#[then("the resulting sr eval Kind should be Boolean true")]
fn then_the_evaluated_term_should_be_boolean_true(world: &mut common::SpecsWorld) {
    assert!(
        world.last_eval_kind == Kind::Lit(Literal::Bool(true)),
        "Expected Boolean true, got {:?}",
        world.last_eval_kind
    );
}

#[then(regex = r#"the resulting eval Kind should be Nat of ([0-9]+)"#)]
#[then(regex = r#"the resulting sr eval Kind should be Nat of ([0-9]+)"#)]
fn then_the_evaluated_value_should_be_nat_of(world: &mut common::SpecsWorld, nat_size: u32) {
    assert!(
        world.last_eval_kind == Kind::Lit(Literal::Nat(nat_size)),
        "Expected Nat of {:?}, got {:?}",
        nat_size,
        world.last_eval_kind
    );
}

#[then(regex = r#"the resulting eval Kind should be Tag of "([^"]*)""#)]
#[then(regex = r#"the resulting sr eval Kind should be Tag of "([^"]*)""#)]
fn then_the_evaluated_value_should_be_tag_of(world: &mut common::SpecsWorld, tag_label: String) {
    assert!(
        world.last_eval_kind == Kind::Lit(Literal::Tag(tag_label.clone())),
        "Expected Tag of {:?}, got {:?}",
        tag_label,
        world.last_eval_kind
    );
}

#[then(regex = r#"the resulting eval Kind should be a fn abs"#)]
#[then(regex = r#"the resulting sr eval Kind should be a fn abs"#)]
fn then_the_evaluated_value_should_be_a_fn_abs(world: &mut common::SpecsWorld) {
    let match_result = match world.last_eval_kind {
        Kind::Abs(_, _) => true,
        _ => false,
    };
    assert!(
        match_result,
        "expected true match result for input, but got false with {:?}",
        world.last_eval_kind
    );
}
