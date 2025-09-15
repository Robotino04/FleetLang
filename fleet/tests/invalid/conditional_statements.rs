use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::{
    assert_compile_error, assert_compile_error_no_formatting, assert_parser_or_tokenizer_error,
};

#[test]
fn non_block_as_body() {
    assert_compile_error_no_formatting(
        indoc! {r##"
            let main = () -> i32 {
                if true let a: i32 = 4;
                return a;
            }
        "##},
        SourceLocation {
            index: 35,
            line: 2,
            column: 12,
        },
    );
}

// TODO: implement this
#[test]
fn if_expression() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                let flag: bool = false;
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
            index: 68,
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
                if true {
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
            index: 120,
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
            if false {
                return 1;
            }
        }
    "##},
        SourceLocation {
            index: 60,
            line: 4,
            column: 4,
        },
    );
}

#[test]
fn not_all_paths_return_elif() {
    assert_compile_error(
        indoc! {r##"
        let main = () -> i32 {
            if false {
                return 1;
            }
            elif true {
                return 3;
            }
        }
    "##},
        SourceLocation {
            index: 100,
            line: 7,
            column: 4,
        },
    );
}
