use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::{assert_compile_error, assert_parser_or_tokenizer_error};

#[test]
fn or_float_as_left() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> bool {
                return 3.0 || true;
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
fn or_float_as_right() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> bool {
                return false || 8.0;
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
fn and_float_as_left() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> bool {
                return 3.0 && true;
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
fn and_float_as_right() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> bool {
                return false && 8.0;
            }
        "##},
        SourceLocation {
            index: 44,
            line: 2,
            column: 20,
        },
    );
}
