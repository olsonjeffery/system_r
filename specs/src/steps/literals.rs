extern crate cucumber;

use cucumber::then;
use system_r::terms::{Kind, Literal};

use crate::common::SpecsWorld;

#[then(regex = r#"the result should be Bytes equal to UTF8 string "([^"]*)""#)]
fn then_the_result_should_be_a_utf8_str(world: &mut SpecsWorld, expected: String) {
    match world.last_eval_kind.clone() {
        Kind::Lit(Literal::Bytes(v)) => {
            // FIXME narrow unescape impl to get test case passing; needs more robust
            // solution for test infra
            let unesc_expected = expected.replace(r#"\r"#, "\r").replace(r#"\n"#, "\n");
            let expected_bytes = unesc_expected.as_bytes();
            assert_eq!(v.as_slice(), expected_bytes);
        }
        v => panic!("Expected eval result to be Bytes, got {:?}", v),
    }
}
