use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::{assert_compile_error, assert_parser_or_tokenizer_error};

#[test]
fn no_initialize() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32;
                return 0;
            }
        "##},
        SourceLocation {
            index: 37,
            line: 2,
            column: 14,
        },
    );
}

#[test]
fn unknown_type() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32asd = 0;
                return a;
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
fn space_in_name() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                let a b: i32 = 0;
                return a;
            }
        "##},
        SourceLocation {
            index: 33,
            line: 2,
            column: 10,
        },
    );
}

#[test]
fn bad_lvalue1() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                a + 3 = 4;
                return a;
            }
        "##},
        SourceLocation {
            index: 53,
            line: 3,
            column: 10,
        },
    );
}

#[test]
fn bad_lvalue2() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                !a = 4;
                return a;
            }
        "##},
        SourceLocation {
            index: 50,
            line: 3,
            column: 7,
        },
    );
}

#[test]
fn missing_semicolon() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 2
                a = a + 4;
                return a;
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
fn missing_colon() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                let a i32 = 2;
                a = a + 4;
                return a;
            }
        "##},
        SourceLocation {
            index: 33,
            line: 2,
            column: 10,
        },
    );
}

#[test]
fn redefine() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 1;
                let a: i32 = 2;
                return a;
            }
        "##},
        SourceLocation {
            index: 51,
            line: 3,
            column: 8,
        },
    );
}

#[test]
fn undefined() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                return a;
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
fn defined_late() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                a = 1 + 2;
                let a: i32 = 0;
                return a;
            }
        "##},
        SourceLocation {
            index: 27,
            line: 2,
            column: 4,
        },
    );
}

#[test]
fn unit_variable() {
    assert_compile_error(
        indoc! {r##"
            let unit_value = () -> () {
                return;
            }
            let main = () -> i32 {
                let a: () = unit_value();
                return 5;
            }
        "##},
        SourceLocation {
            index: 76,
            line: 5,
            column: 11,
        },
    );
}

#[test]
fn i32_assigned_to_bool() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> bool {
                let a: bool = 4;
                return a;
            }
        "##},
        SourceLocation {
            index: 28,
            line: 2,
            column: 4,
        },
    );
}

#[test]
fn bool_assigned_to_i32() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = true;
                return a;
            }
        "##},
        SourceLocation {
            index: 27,
            line: 2,
            column: 4,
        },
    );
}
