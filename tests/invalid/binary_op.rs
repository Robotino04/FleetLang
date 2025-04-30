use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::assert_parser_or_tokenizer_error;

#[test]
fn paren_malformed() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return 2 (- 3);
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
fn missing_left_operand() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return / 3;
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
fn missing_right_operand() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return 1 + ;
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
fn missing_semicolon() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return 2 * 2
            }
        "##},
        SourceLocation {
            index: 40,
            line: 3,
            column: 0,
        },
    );
}

#[test]
fn unequal_parens_more_open() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return ((((987));
            }
        "##},
        SourceLocation {
            index: 43,
            line: 2,
            column: 20,
        },
    );
}

#[test]
fn unequal_parens_more_closed() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return ((52))));
            }
        "##},
        SourceLocation {
            index: 40,
            line: 2,
            column: 17,
        },
    );
}
