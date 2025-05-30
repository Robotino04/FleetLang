use indoc::indoc;

use crate::common::assert_compile_and_return_value;

#[test]
fn main_returning_0() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 0;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn main_returning_5() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 5;
            }
        "##},
        "main",
        5,
    );
}

#[test]
fn two_functions() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 5;
            }
            let main2 = () -> i32 {
                return 1;
            }
        "##},
        "main",
        5,
    );
}

#[test]
fn two_functions_reverse() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main2 = () -> i32 {
                return 5;
            }
            let main = () -> i32 {
                return 1;
            }
        "##},
        "main",
        1,
    );
}
