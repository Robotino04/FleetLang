use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::assert_compile_error;

#[test]
fn return_wrong_size() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a: i16 = 2;
                return a;
            }
        "##},
        SourceLocation {
            index: 54,
            line: 3,
            column: 11,
        },
    );
}

#[test]
fn initialize_wrong_size() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i16 {
                let a: i8 = 2;
                let b: i16 = a;
                return b;
            }
        "##},
        SourceLocation {
            index: 59,
            line: 3,
            column: 17,
        },
    );
}

#[test]
fn assign_wrong_size() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i16 {
                let a: i64 = 2;
                let b: i16 = 6;
                b = a;
                return b;
            }
        "##},
        SourceLocation {
            index: 71,
            line: 4,
            column: 8,
        },
    );
}

#[test]
fn arg_wrong_size() {
    assert_compile_error(
        indoc! {r##"
            let foo = (a: i8) -> () {}
            let main = () -> i64 {
                let a: i64 = 2;
                foo(a);
                return a;
            }
        "##},
        SourceLocation {
            index: 78,
            line: 4,
            column: 8,
        },
    );
}

#[test]
fn call_wrong_size() {
    assert_compile_error(
        indoc! {r##"
            let foo = (a: i8) -> i8 {
                return a;
            }
            let main = () -> i32 {
                let a: i8 = 2;
                return foo(a);
            }
        "##},
        SourceLocation {
            index: 95,
            line: 6,
            column: 11,
        },
    );
}
