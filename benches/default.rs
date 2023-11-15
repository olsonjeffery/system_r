#![feature(test)]
#![deny(warnings)]
extern crate test;

mod platform_bindings;

#[cfg(test)]
mod bench {
    use system_r::{testing, platform_bindings::PlatformBindings, types::Context, terms::Kind};
    use test::{black_box, Bencher};

    #[bench]
    fn bench_pow(b: &mut Bencher) {
        // Optionally include some setup
        let x: f64 = 2211.0 * 11.0;
        let y: f64 = 3401.0 * 103.0;

        b.iter(|| {
            // Inner closure, the actual test
            for _i in 1..100 {
                black_box(x.powf(y).powf(x));
            }
        });
    }

    #[bench]
    fn fib_7_bottom_p2e(b: &mut Bencher) {
        let code = r#"
let fib = \z: Nat->Nat. \i: Nat.
        case i of
        | 1 => 1
        | 0 => 1
        | n => natAdd( z(natSub(n, 1)), z(natSub(n, 2)) ) in
(fix fib)(7);
        "#;
        let mut pb = PlatformBindings::default();
        pb.register("natAdd", crate::platform_bindings::arith::pb_add());
        pb.register("natSub", crate::platform_bindings::arith::pb_sub());

        b.iter(|| {
            let mut t = match testing::parse_single_block(&pb, &code) {
                Ok(t) => t,
                _ => return
            };

            let mut ctx = Context::default();
            ctx.platform_bindings = pb.clone();

            let result = match testing::type_check_and_eval_single_block(&mut ctx, &mut t, code, false) {
                Ok(t) => t,
                _ => return
            };
            assert_eq!(result.1.kind, Kind::Lit(system_r::terms::Literal::Nat(21)));
            println!("end of bench with result: {:?}", result);
        });
    }
}
