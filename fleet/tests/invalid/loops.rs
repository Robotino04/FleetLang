use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::{assert_compile_error, assert_parser_or_tokenizer_error};

#[test]
fn break_outside_loop() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                break;
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
fn for_wrong_condition() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                for (a = 0; nonexistent < 3; a = a + 1) {
                    a = a * 2;
                }
                return a;
            }
        "##},
        SourceLocation {
            index: 59,
            line: 3,
            column: 16,
        },
    );
}

#[test]
fn for_wrong_increment() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                for (a = 0; a < 3; a = nonexistent + 1) {
                    a = a * 2;
                }
                return a;
            }
        "##},
        SourceLocation {
            index: 70,
            line: 3,
            column: 27,
        },
    );
}

#[test]
fn out_of_scope_while() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                while 1 {
                    let a: i32 = 2;
                }
                return a;
            }
        "##},
        SourceLocation {
            index: 78,
            line: 5,
            column: 11,
        },
    );
}

#[test]
fn paren_mismatch() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 2; ; )) {
                    let a: i32 = 0;
                }
            }
        "##},
        SourceLocation {
            index: 51,
            line: 2,
            column: 28,
        },
    );
}

#[test]
fn skip_not_in_loop() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                skip;
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
fn statement_in_condition() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                while let a: i32 = 0 {
                    2;
                }
            }
        "##},
        SourceLocation {
            index: 33,
            line: 2,
            column: 10,
        },
    );
}

#[test]
fn too_few_for_clauses() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 2; i < 3) {
                    3;
                }
                return 0;
            }
        "##},
        SourceLocation {
            index: 53,
            line: 2,
            column: 30,
        },
    );
}

#[test]
fn too_many_for_clauses() {
    assert_parser_or_tokenizer_error(
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 2; i < 3; i = i + 1; i > 4) {
                    3;
                }
                return 0;
            }
        "##},
        SourceLocation {
            index: 64,
            line: 2,
            column: 41,
        },
    );
}

#[test]
fn i32_as_while_condition() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                while 3 {
                }
                return 0;
            }
        "##},
        SourceLocation {
            index: 33,
            line: 2,
            column: 10,
        },
    );
}

#[test]
fn i32_as_for_condition() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 2; i; i = i + 1) {
                }
                return 0;
            }
        "##},
        SourceLocation {
            index: 48,
            line: 2,
            column: 25,
        },
    );
}
