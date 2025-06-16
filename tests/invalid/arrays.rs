use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::{
    assert_compile_error, assert_compile_error_no_formatting, assert_parser_or_tokenizer_error,
};

#[test]
fn literal_mixed_elements() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                [1 as i32, 9 + 2, 2 - 5 * 2 - 1, 8 as i64];
                return 0;
            }
        "##},
        SourceLocation {
            index: 60,
            line: 2,
            column: 37,
        },
    );
}

#[test]
fn missing_bracket_sized_type() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32[5 = [1, 2, 3, 4, 5];
                return 0;
            }
        "##},
        SourceLocation {
            index: 40,
            line: 2,
            column: 17,
        },
    );
}

#[test]
fn missing_bracket_unsized_type() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32[ = [1, 2, 3, 4, 5];
                return 0;
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
fn wrong_type_assigned() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32[] = [5 as i64, 4, 3, 2, 1];
                return a[1];
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
fn wrong_size_assigned() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32[3] = [5, 4, 3, 2, 1];
                return a[1];
            }
        "##},
        SourceLocation {
            index: 27,
            line: 2,
            column: 4,
        },
    );
}
