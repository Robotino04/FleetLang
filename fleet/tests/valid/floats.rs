use std::f32;

use indoc::indoc;

use crate::common::assert_compile_and_return_value;

#[test]
fn positive_inf() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return 1 / 0;
            }
        "##},
        "main",
        // NOTE: this only works in testing. Converting infinity to an int (which
        // the generated main does) is undefined
        f32::INFINITY,
    );
}

#[test]
fn negative_inf() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return -1 / 0;
            }
        "##},
        "main",
        // NOTE: this only works in testing. Converting infinity to an int (which
        // the generated main does) is undefined
        f32::NEG_INFINITY,
    );
}

#[test]
fn negative_inf_neg_zero() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return 1 / -0;
            }
        "##},
        "main",
        // NOTE: this only works in testing. Converting infinity to an int (which
        // the generated main does) is undefined
        f32::NEG_INFINITY,
    );
}

#[test]
fn positive_inf_neg_zero() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return -1 / -0;
            }
        "##},
        "main",
        // NOTE: this only works in testing. Converting infinity to an int (which
        // the generated main does) is undefined
        f32::INFINITY,
    );
}

#[test]
fn nan_not_equal() {
    assert_compile_and_return_value(
        indoc! {r##"
            let inf = () -> f32 {
                return 1 / 0;
            }
            let nan = () -> f32 {
                return inf() - inf();
            }
            let main = () -> bool {
                return nan() != nan();
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn nan_equal() {
    assert_compile_and_return_value(
        indoc! {r##"
            let inf = () -> f32 {
                return 1 / 0;
            }
            let nan = () -> f32 {
                return inf() - inf();
            }
            let main = () -> bool {
                return nan() == nan();
            }
        "##},
        "main",
        false,
    );
}

#[test]
fn not_inf() {
    assert_compile_and_return_value(
        indoc! {r##"
            let inf = () -> f32 {
                return 1 / 0;
            }
            let main = () -> bool {
                return !inf();
            }
        "##},
        "main",
        false,
    );
}
#[test]
fn not_nan() {
    assert_compile_and_return_value(
        indoc! {r##"
            let inf = () -> f32 {
                return 1 / 0;
            }
            let nan = () -> f32 {
                return inf() - inf();
            }
            let main = () -> bool {
                return !nan();
            }
        "##},
        "main",
        false,
    );
}

#[test]
fn inf_equal() {
    assert_compile_and_return_value(
        indoc! {r##"
            let inf = () -> f32 {
                return 1 / 0;
            }
            let main = () -> bool {
                return inf() == inf();
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn inf_not_equal() {
    assert_compile_and_return_value(
        indoc! {r##"
            let inf = () -> f32 {
                return 1 / 0;
            }
            let main = () -> bool {
                return inf() != inf();
            }
        "##},
        "main",
        false,
    );
}

#[test]
fn zero_over_zero() {
    assert_compile_and_return_value(
        indoc! {r##"
            let isnan = (x: f32) -> bool {
                return x != x;
            }
            let main = () -> bool {
                return isnan(0 / 0);
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn nanpropagation_add() {
    assert_compile_and_return_value(
        indoc! {r##"
            let isnan = (x: f32) -> bool {
                return x != x;
            }
            let nan = () -> f32 {
                return 0 / 0;
            }
            let main = () -> bool {
                return isnan(nan() + 0.5);
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn nanpropagation_sub() {
    assert_compile_and_return_value(
        indoc! {r##"
            let isnan = (x: f32) -> bool {
                return x != x;
            }
            let nan = () -> f32 {
                return 0 / 0;
            }
            let main = () -> bool {
                return isnan(nan() - 0.5);
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn nanpropagation_mul() {
    assert_compile_and_return_value(
        indoc! {r##"
            let isnan = (x: f32) -> bool {
                return x != x;
            }
            let nan = () -> f32 {
                return 0 / 0;
            }
            let main = () -> bool {
                return isnan(nan() * 0.5);
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn nanpropagation_div() {
    assert_compile_and_return_value(
        indoc! {r##"
            let isnan = (x: f32) -> bool {
                return x != x;
            }
            let nan = () -> f32 {
                return 0 / 0;
            }
            let main = () -> bool {
                return isnan(nan() / 0.5);
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn plus_zero_equals_minus_zero() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return 0.0 == -0.0;
            }
        "##},
        "main",
        true,
    );
}
