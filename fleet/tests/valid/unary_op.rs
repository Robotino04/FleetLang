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
        "main",
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
        "main",
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
        "main",
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
