extern crate cucumber;

use core::fmt;

use cucumber::{gherkin::Step, given, then, when};

use system_r::{
    platform_bindings::WrappedContent,
    terms::{Kind, Literal, Term, ExtKind, ExtTerm},
    testing::{self, code_format},
    types::Context,
    types::Type, extensions::{tylet::{TyLetContext, TyLetPattern, TyLetParser, TyLetExtension}, SystemRExtension}, syntax::parser::ExtParser, diagnostics::Diagnostic,
};

use crate::common::{self, extensions::{OmniContext, OmniParser, OmniKind, OmniTerm}, SpecsWorld};

static TYLET_CTX_NAME: &'static str = "TyLet";

pub fn parse_for_extension<'s, TExtTokenKind: fmt::Debug + Default + Clone + PartialEq + PartialOrd,
        TExtKind: fmt::Debug + Default + Clone + PartialEq + PartialOrd,
        TEXtPat: fmt::Debug + Default + Clone + PartialEq + PartialOrd,
        TLE: Clone + SystemRExtension<TExtTokenKind, TExtKind, TEXtPat>>(input: &str, mut parser: ExtParser<TExtTokenKind, TExtKind, TEXtPat, TLE>, world: &mut SpecsWorld) -> Result<ExtTerm<TEXtPat, TExtKind>, Diagnostic> {
    testing::operate_parser_for(parser, input)
}

#[given(regex = r#"^a system_r toolchain extended for tylet"#)]
fn given_a_new_tylet_context(world: &mut common::SpecsWorld) {
    world.contexts.insert(TYLET_CTX_NAME.to_owned(), OmniContext::TyLet(TyLetContext::default()));
}

#[then("the last ext should parse successfully")]
fn then_the_last_ext_should_parse_successfully(world: &mut common::SpecsWorld) {
    assert!(world.last_ext_parse_success, "fail msg: {:?}", world.last_ext_parse_msg);
}

#[when("it is processed for the tylet extension")]
fn when_it_is_processed_for_tylet(world: &mut common::SpecsWorld) {
    let input = world.code_snippet.clone();
    let ext = TyLetExtension {};
    let pb = world.platform_bindings.clone();
    let parser = TyLetParser::new(&pb, &input, ext);
    let res = parse_for_extension(&input, parser, world);
    let (term, kind) = match res {
        Ok(t) => {
            let k = OmniKind::TyLet(t.clone().kind);
            let t = OmniTerm::TyLet(t);
            (t, k)
        },
        Err(e) => {
            world.last_ext_parse_msg = format!("error diag: {:?}", e);
            (OmniTerm::Empty, OmniKind::Empty)
        }
    };
    world.last_ext_parse_success = if kind == OmniKind::Empty { false } else { true };
    world.last_ext_parse_kind = kind;
    world.last_ext_parse_term = term;
    //when_eval_is_ran(world);
}