#![feature(test)]
#![deny(warnings)]
#![dent(clippy::all)]
extern crate chrono;
extern crate cucumber;
extern crate test;

use cucumber::{writer, World, WriterExt};
use std::fs;

mod steps;

mod common;

fn main() {
    blocking_specs_run();
}

pub fn blocking_specs_run() {
    //futures::executor::block_on(common::SpecsWorld::run("tests/features"));
    //let ts = chrono::Utc::now();
    //let ts = ts.format("%Y%m%d%H%M%S%.f");
    let file = fs::File::create(dbg!(format!("./specs-results.json"))).unwrap();

    let json_writer = writer::Json::new(file);

    futures::executor::block_on(
        common::SpecsWorld::cucumber()
            .with_writer(
                writer::Basic::stdout()
                    .summarized()
                    .tee::<common::SpecsWorld, _>(json_writer)
                    .normalized(),
            )
            .run("tests/features"),
    );
}
