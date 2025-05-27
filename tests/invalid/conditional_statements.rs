use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::{assert_compile_error, assert_parser_or_tokenizer_error};

#[test]
fn non_block_as_body() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                if 5 let a: i32 = 4;
                return a;
            }
        "##},
        SourceLocation {
            index: 32,
            line: 2,
            column: 9,
        },
    );
}

// TODO: implement this
#[test]
fn if_expression() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                let flag: i32 = 0;
                let a: i32 = if flag {
                    2;
                }
                else {
                    3;
                }
                return a;
            }
        "##},
        SourceLocation {
            index: 63,
            line: 3,
            column: 17,
        },
    );
}

#[test]
fn double_else() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                if 1 {
                    return 1;
                }
                else {
                    return 2;
                }
                else {
                    return 3;
                }
            }
        "##},
        SourceLocation {
            index: 117,
            line: 9,
            column: 4,
        },
    );
}

#[test]
fn not_all_paths_return_if() {
    assert_compile_error(
        indoc! {r##"
        let main = () -> i32 {
            if 0 {
                return 1;
            }
        }
    "##},
        SourceLocation {
            index: 21,
            line: 1,
            column: 21,
        },
    );
}

#[test]
fn not_all_paths_return_elif() {
    assert_compile_error(
        indoc! {r##"
        let main = () -> i32 {
            if 0 {
                return 1;
            }
            elif 1 {
                return 3;
            }
        }
    "##},
        SourceLocation {
            index: 21,
            line: 1,
            column: 21,
        },
    );
}
