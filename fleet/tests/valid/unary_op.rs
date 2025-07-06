use indoc::indoc;

use crate::common::assert_compile_and_return_value;

#[test]
fn bitwise_not_0() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return ~0;
            }
        "##},
        "main",
        -1,
    );
}

#[test]
fn bitwise_not_12() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return ~12;
            }
        "##},
        "main",
        -13,
    );
}

#[test]
fn negative_5() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return -5;
            }
        "##},
        "main",
        -5,
    );
}

#[test]
fn not_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return !false;
            }
        "##},
        // HACK: this doesn't initialize the FL runtime and is only done so we aren't
        // limited to main returning an i32. Tests should whenever possible call real main
        "fleet_main",
        true,
    );
}

#[test]
fn not_true() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return !true;
            }
        "##},
        // HACK: this doesn't initialize the FL runtime and is only done so we aren't
        // limited to main returning an i32. Tests should whenever possible call real main
        "fleet_main",
        false,
    );
}

#[test]
fn nested_1() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return !-3;
            }
        "##},
        // HACK: this doesn't initialize the FL runtime and is only done so we aren't
        // limited to main returning an i32. Tests should whenever possible call real main
        "fleet_main",
        false,
    );
}

#[test]
fn nested_2() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return -~0;
            }
        "##},
        "main",
        1,
    );
}
