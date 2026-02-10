use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::{assert_compile_error, assert_parser_or_tokenizer_error};

#[test]
fn missing_operand() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return !;
            }
        "##},
        SourceLocation {
            index: 35,
            line: 2,
            column: 12,
        },
    );
}
#[test]
fn missing_semicolon() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return !5
            }
        "##},
        SourceLocation {
            index: 37,
            line: 3,
            column: 0,
        },
    );
}

#[test]
fn mixed_missing_operand() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return !~;
            }
        "##},
        SourceLocation {
            index: 36,
            line: 2,
            column: 13,
        },
    );
}
#[test]
fn minus_after_operand() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return 4-;
            }
        "##},
        SourceLocation {
            index: 36,
            line: 2,
            column: 13,
        },
    );
}

#[test]
fn binary_not_bool() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> bool {
                return ~true;
            }
        "##},
        SourceLocation {
            index: 36,
            line: 2,
            column: 12,
        },
    );
}

#[test]
fn minus_bool() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> bool {
                return -false;
            }
        "##},
        SourceLocation {
            index: 36,
            line: 2,
            column: 12,
        },
    );
}
