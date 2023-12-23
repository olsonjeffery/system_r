extern crate cucumber;

use cucumber::{gherkin::Step, given, then, when};

use system_r::{
    bottom::BottomTokenKind,
    feedback::{syntax::ParserError, type_check::TypeCheckerDiagnosticInfo},
    terms::{Kind, Literal, Term},
    testing,
    type_check::Type,
    type_check::TypeChecker,
};

use crate::common::{self, extensions::OmniTypeChecker};

pub static BOTTOM_TC_NAME: &'static str = "Bottom";

#[given(regex = r#"^a new type checker"#)]
#[given(regex = r#"^a new Bottom type checker"#)]
pub fn given_a_new_type_checker(world: &mut common::SpecsWorld) {
    let new_tc = TypeChecker::default();
    world
        .type_checkers
        .insert(BOTTOM_TC_NAME.to_string(), OmniTypeChecker::Bottom(new_tc));
}

#[given(regex = r#"^a code block:(\w?.*)$"#)]
#[given(regex = r#"^a srpt block:(\w?.*)$"#)]
#[given(regex = r#"^a system_r plain text block:(\w?.*)$"#)]
fn given_a_code_snippet(world: &mut common::SpecsWorld, step: &Step) {
    world.code_snippet = step.docstring.clone().unwrap();
}

#[given(regex = r#"adding an instrinsic named iiiNatAdd to the "([^"]*)" context"#)]
fn given_an_intrinsic_for_nat_addition_named_i_nat_add_to_ctx(world: &mut common::SpecsWorld, ctx_name: String) {
    let pb = &mut world.platform_bindings;

    pb.register("iiiNatAdd", common::platform_bindings::arith::pb_add());

    world
        .type_checkers
        .get_mut(&ctx_name)
        .unwrap()
        .set_platform_bindings(pb.clone());
}

#[given(regex = r#"platform bindings for Nat add & sub"#)]
fn given_platform_bindings_for_nat_add_and_sub(world: &mut common::SpecsWorld) {
    let pb = &mut world.platform_bindings;

    pb.register("natAdd", common::platform_bindings::arith::pb_add());
    pb.register("natSub", common::platform_bindings::arith::pb_sub());

    world
        .type_checkers
        .get_mut(BOTTOM_TC_NAME)
        .unwrap()
        .set_platform_bindings(pb.clone());
}

#[given(regex = r#"an instrinsic for Nat addition named iiiNatAdd"#)]
fn given_an_intrinsic_for_nat_addition_named_i_nat_add(world: &mut common::SpecsWorld) {
    given_an_intrinsic_for_nat_addition_named_i_nat_add_to_ctx(world, BOTTOM_TC_NAME.to_owned());
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
    let parse_result = match parse_result {
        Ok(term) => term,
        Err(e) => {
            world.last_parse_success = false;
            world.last_parse_kind = Kind::Lit(Literal::Unit);
            let output = match e.downcast_ref::<ParserError<BottomTokenKind>>() {
                Some(e) => e.to_string(),
                None => match e.downcast_ref() {
                    Some(TypeCheckerDiagnosticInfo { .. }) => "diagnostic error".to_owned(),
                    None => "unknown".to_owned(),
                },
            };
            world.last_parse_msg = output.to_owned();
            world.last_parse_term = Term::unit();
            return;
        }
    };
    world.last_parse_success = true;
    world.last_parse_term = parse_result.clone();
    world.last_parse_kind = parse_result.kind;
    world.last_parse_msg = "".to_owned();
}

#[when("eval is ran")]
#[when("type_check and eval for BottomDialect is ran")]
#[when("sr evals it")]
fn when_eval_is_ran(world: &mut common::SpecsWorld) {
    if world.code_snippet == "" {
        assert!(false, "passed empty code snippet to parser");
    }
    let Some(OmniTypeChecker::Bottom(ctx)) = world.type_checkers.get_mut(BOTTOM_TC_NAME) else {
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
            world.last_eval_msg = e.to_string();
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

#[then(regex = r#"the last parse message should contain "([^"]*)""#)]
fn then_the_last_eval_message_should_container(world: &mut common::SpecsWorld, expected_snippet: String) {
    assert!(
        world.last_parse_msg.contains(&expected_snippet),
        "Expected last parse msg: {:?} to contain snippet {:?}, didn't",
        world.last_parse_msg,
        expected_snippet
    );
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

#[then("the result should be Boolean true")]
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
fn then_the_evaluated_value_should_be_nat_of(world: &mut common::SpecsWorld, nat_size: u64) {
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

#[then(regex = r#"the final value after eval should equal: "([^"]*)""#)]
fn then_the_final_value_after_eval_should_equal(world: &mut common::SpecsWorld, snippet: String) {
    let first_final_value = world.last_eval_kind.clone();

    world.last_parse_success = false;
    world.code_snippet = snippet;
    when_it_is_parsed(world);
    if world.last_parse_success {
        world.type_checkers.remove(BOTTOM_TC_NAME);
        given_a_new_type_checker(world);
        when_eval_is_ran(world);

        if world.last_eval_success {
            let left = first_final_value;
            let right = world.last_eval_kind.clone();
            let left_dbg = format!("{:?}", left);
            let right_dbg = format!("{:?}", right);
            assert_eq!(
                left_dbg, right_dbg,
                "Comparing final value after eval (left) to provided input snippet (right) failed."
            );
            return;
        }
        assert!(false, "failed; shouldn't reach here");
    }
    assert!(false, "failed; shouldn't reach here");
}
