use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::{assert_compile_error, assert_compile_error_no_formatting, assert_parser_or_tokenizer_error};

#[test]
fn missing_main() {
    assert_compile_error(
        indoc! {r##"
            let foo = () -> i32 {
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
            let main = () -> i32 {
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
    assert_compile_error_no_formatting(
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

#[test]
fn too_few_arguments() {
    assert_compile_error(
        indoc! {r##"
            let foo = (a: i32) -> i32 {
                return a + 3;
            }
            let main = () -> i32 {
                return foo();
            }
        "##},
        SourceLocation {
            index: 86,
            line: 5,
            column: 15,
        },
    );
}

#[test]
fn c_like_declaration() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let foo = (a: i32) -> i32;

            let main = () -> i32 {
                return 5;
            }
            let foo = (a: i32) -> i32 {
                return 4;
            }
        "##},
        SourceLocation {
            index: 25,
            line: 1,
            column: 25,
        },
    );
}

#[test]
fn redefine_function() {
    assert_compile_error(
        indoc! {r##"
            let foo = () -> i32 {
                return 3;
            }
            let main = () -> i32 {
                return foo();
            }
            let foo = () -> i32 {
                return 4;
            }
        "##},
        SourceLocation {
            index: 81,
            line: 7,
            column: 0,
        },
    );
}

#[test]
fn too_many_arguments() {
    assert_compile_error(
        indoc! {r##"
            let foo = (a: i32) -> i32 {
                return a + 1;
            }
            let main = () -> i32 {
                return foo(1, 2);
            }
        "##},
        SourceLocation {
            index: 89,
            line: 5,
            column: 18,
        },
    );
}
