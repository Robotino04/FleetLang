use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::{assert_compile_and_return_value, assert_compile_and_warning};

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

#[test]
fn missing_variable_type_int() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = 123;
                return a;
            }
        "##},
        "main",
        123i32,
    );
}

#[test]
fn chained_untyped_assignment() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = 123;
                let b = a;
                let c = b;
                let d = c;
                return d;
            }
        "##},
        "main",
        123i32,
    );
}

#[test]
fn chained_as_idk_casts() {
    assert_compile_and_warning(
        indoc! {r##"
            let main = () -> i32 {
                return 2 as idk as idk as idk;
            }
        "##},
        SourceLocation {
            index: 34,
            line: 2,
            column: 11,
        },
    );
}

#[test]
fn as_bool_dont_truncate() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 2 as bool as i32;
            }
        "##},
        "main",
        1,
    );
}
