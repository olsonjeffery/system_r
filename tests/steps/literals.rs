extern crate cucumber;

use cucumber::{when, then};
use system_r::terms::{Kind, Literal};

use crate::common::SpecsWorld;

#[when("panic with bytes")]
fn panic_with_bytes(_world: &mut SpecsWorld) {
    let cont_as_str = "GET / HTTP/1.1\r\nHost: www.example.com\r\nConnection: close\r\n\r\n";
    let cons_bytes = cont_as_str.as_bytes();
    let bytes_len = cons_bytes.len();
    panic!("choking on string: {:?} with bytes: {:?} lengths: {:?} {:?}", cont_as_str, cons_bytes, 0, bytes_len);
}

#[then(regex = r#"the result should be Bytes equal to UTF8 string "([^"]*)""#)]
fn then_the_result_should_be_a_utf8_str(world: &mut SpecsWorld, expected: String) {
    match world.last_eval_kind.clone() {
        Kind::Lit(Literal::Bytes(v)) => {
            // FIXME narrow unescape impl to get test case passing; needs more robust solution for test infra
            let unesc_expected = expected.replace(r#"\r"#, "\r").replace(r#"\n"#, "\n");
            let expected_bytes = unesc_expected.as_bytes();
            assert_eq!(v.as_slice(), expected_bytes);
        },
        v => panic!("Expected eval result to be Bytes, got {:?}", v)
    }
}