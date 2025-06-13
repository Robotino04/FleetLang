use indoc::indoc;

use crate::common::assert_compile_and_return_value;

#[test]
fn missing_return_type() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> {
                return 0;
            }
        "##},
        "main",
        0i32,
    );
}

#[test]
fn missing_return_type_chained() {
    assert_compile_and_return_value(
        indoc! {r##"
            let foo = (a: i16) -> {
                return a;
            }
            let main = () -> {
                return foo(5);
            }
        "##},
        "main",
        5i16,
    );
}

#[test]
fn missing_variable_type() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> {
                let a = 2 < 9;
                return a;
            }
        "##},
        "main",
        true,
    );
}
