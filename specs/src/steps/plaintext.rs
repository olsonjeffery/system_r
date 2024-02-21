use cucumber::{then, when};
use system_r::{dialect::bottom::BottomExtension, terms::plaintext::Plaintext};

use crate::common;

#[when("it is converted to plaintext")]
fn when_it_is_converted_to_plaintext(world: &mut common::SpecsWorld) {
    let term = world.last_parse_term.clone();
    match term.to_plaintext(&BottomExtension) {
        Ok(s) => world.last_plaintext = s,
        Err(e) => {
            world.last_plaintext = format!("plaintext failed: {e:?}");
        }
    };
}

#[then("the plaintext should match the input code block")]
fn then_the_last_tyck_should_be_successful(world: &mut common::SpecsWorld) {
    assert!(world.last_plaintext == world.code_snippet, "expected {} got {}", world.code_snippet, world.last_plaintext);
}