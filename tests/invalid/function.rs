use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::{assert_compile_error, assert_parser_or_tokenizer_error};

#[test]
fn missing_main() {
    assert_compile_error(
        indoc! {r##"
        "##},
        SourceLocation {
            index: 0,
            line: 1,
            column: 0,
        },
    );
}

#[test]
fn missing_let() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            main = () -> i32 {
                return 0;
            }
        "##},
        SourceLocation {
            index: 0,
            line: 1,
            column: 0,
        },
    );
}

#[test]
fn missing_name() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let = () -> i32 {
                return 0;
            }
        "##},
        SourceLocation {
            index: 4,
            line: 1,
            column: 4,
        },
    );
}

#[test]
fn missing_equal() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main () -> i32 {
                return 0;
            }
        "##},
        SourceLocation {
            index: 9,
            line: 1,
            column: 9,
        },
    );
}

#[test]
fn missing_open_paren() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = ) -> i32 {
                return 0;
            }
        "##},
        SourceLocation {
            index: 11,
            line: 1,
            column: 11,
        },
    );
}

#[test]
fn missing_close_paren() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = ( -> i32 {
                return 0;
            }
        "##},
        SourceLocation {
            index: 13,
            line: 1,
            column: 13,
        },
    );
}

#[test]
fn missing_right_arrow() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () i32 {
                return 0;
            }
        "##},
        SourceLocation {
            index: 14,
            line: 1,
            column: 14,
        },
    );
}

#[test]
fn missing_return_type() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> {
                return 0;
            }
        "##},
        SourceLocation {
            index: 17,
            line: 1,
            column: 17,
        },
    );
}

#[test]
fn missing_open_brace() {
    // Parsing fails at the closing brace because a statement is also a valid function body
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32
                return 0;
            }
        "##},
        SourceLocation {
            index: 35,
            line: 3,
            column: 0,
        },
    );
}

#[test]
fn missing_close_brace() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                return 0;
        "##},
        SourceLocation {
            index: 35,
            line: 2,
            column: 12,
        },
    );
}

#[test]
fn unknown_characters_1() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let §§§
        "##},
        SourceLocation {
            index: 4,
            line: 1,
            column: 4,
        },
    );
}

#[test]
fn unknown_characters_2() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let §§§
            `
        "##},
        SourceLocation {
            index: 8,
            line: 2,
            column: 0,
        },
    );
}

#[test]
fn missing_return() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {}
        "##},
        SourceLocation {
            index: 0,
            line: 1,
            column: 0,
        },
    );
}

#[test]
fn undefined_function() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                return nonexistent();
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
fn non_block_as_body() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32
                return 0;
        "##},
        SourceLocation {
            index: 25,
            line: 2,
            column: 4,
        },
    );
}
