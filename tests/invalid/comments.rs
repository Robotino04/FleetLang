use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::assert_parser_or_tokenizer_error;

#[test]
fn missing_left_block_delimiter() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return 5; */
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
fn missing_right_block_delimiter() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                /* return 5;
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
fn spaced_right_block_delimiter() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                /* return 5; * /
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
fn spaced_left_block_delimiter() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                / * return 5; */
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
fn spaced_line_delimiter() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                / / return 5;
            }
        "##},
        SourceLocation {
            index: 27,
            line: 2,
            column: 4,
        },
    );
}
