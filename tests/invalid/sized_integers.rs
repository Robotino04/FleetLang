use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::{assert_compile_error, assert_parser_or_tokenizer_error};

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
            index: 47,
            line: 3,
            column: 4,
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
            index: 46,
            line: 3,
            column: 4,
        },
    );
}

#[test]
fn assign_wrong_size() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i16 {
                let a: i64 = 2;
                let b: i16 = a;
                b = a;
                return b;
            }
        "##},
        SourceLocation {
            index: 67,
            line: 4,
            column: 4,
        },
    );
}

#[test]
fn arg_wrong_size() {
    assert_compile_error(
        indoc! {r##"
            let foo = (a: i8) -> () {
            }
            let main = () -> i64 {
                let a: i64 = 2;
                foo(a);
                return a;
            }
        "##},
        SourceLocation {
            index: 79,
            line: 5,
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
            index: 88,
            line: 6,
            column: 4,
        },
    );
}
