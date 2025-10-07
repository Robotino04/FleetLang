use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::{assert_compile_error, assert_parser_or_tokenizer_error};

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

#[test]
fn mixed_bool_int() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, true, 3];
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
fn bool_index() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3];
                return a[true];
            }
        "##},
        SourceLocation {
            index: 59,
            line: 3,
            column: 13,
        },
    );
}

#[test]
fn float_index() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3];
                return a[2.7];
            }
        "##},
        SourceLocation {
            index: 59,
            line: 3,
            column: 13,
        },
    );
}

#[test]
fn signed_index() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3];
                return a[2 as i16];
            }
        "##},
        SourceLocation {
            index: 59,
            line: 3,
            column: 13,
        },
    );
}
#[test]
fn length_number() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a = 5;
                return @length(a);
            }
        "##},
        SourceLocation {
            index: 57,
            line: 3,
            column: 19,
        },
    );
}
#[test]
fn length_struct() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a = struct {
                    a: i32,
                } {
                    a: 9,
                };
                return @length(a);
            }
        "##},
        SourceLocation {
            index: 108,
            line: 7,
            column: 19,
        },
    );
}
#[test]
fn length_bool() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                return @length(true);
            }
        "##},
        SourceLocation {
            index: 42,
            line: 2,
            column: 19,
        },
    );
}
