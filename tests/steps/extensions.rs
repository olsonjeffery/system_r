extern crate cucumber;

use cucumber::{gherkin::Step, given, then, when};

use system_r::{
    platform_bindings::WrappedContent,
    terms::{Kind, Literal, Term},
    testing::{self, code_format},
    types::Context,
    types::Type,
};

use crate::common;

static CTX_NAME: &'static str = "L";

#[given(regex = r#"^a system_r toolchain extended for tylet"#)]
fn given_a_new_context(world: &mut common::SpecsWorld) {
}

#[when("it is processed for the tylet extension")]
fn when_it_is_processed_for_tylet(world: &mut common::SpecsWorld) {

}