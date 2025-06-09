use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::{assert_compile_error, assert_parser_or_tokenizer_error};

#[test]
fn missing_left_operand() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return <= 2;
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
fn missing_middle_operand() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return 1 < > 2;
            }
        "##},
        SourceLocation {
            index: 38,
            line: 2,
            column: 15,
        },
    );
}

#[test]
fn missing_right_operand() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return 3 && ;
            }
        "##},
        SourceLocation {
            index: 39,
            line: 2,
            column: 16,
        },
    );
}

#[test]
fn missing_semicolon() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return 3 || 7
            }
        "##},
        SourceLocation {
            index: 41,
            line: 3,
            column: 0,
        },
    );
}

#[test]
fn or_int_as_left() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> bool {
                return 3 || true;
            }
        "##},
        SourceLocation {
            index: 35,
            line: 2,
            column: 11,
        },
    );
}

#[test]
fn or_int_as_right() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> bool {
                return false || 8;
            }
        "##},
        SourceLocation {
            index: 44,
            line: 2,
            column: 20,
        },
    );
}

#[test]
fn and_int_as_left() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> bool {
                return 3 && true;
            }
        "##},
        SourceLocation {
            index: 35,
            line: 2,
            column: 11,
        },
    );
}

#[test]
fn and_int_as_right() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> bool {
                return false && 8;
            }
        "##},
        SourceLocation {
            index: 44,
            line: 2,
            column: 20,
        },
    );
}
