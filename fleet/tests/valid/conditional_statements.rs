use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::{
    assert_compile_and_return_value, assert_compile_and_warning, assert_compile_error,
};

#[test]
fn if_else() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                if a != 0 {
                    return 1;
                }
                else {
                    return 2;
                }
            }
        "##},
        "main",
        2,
    );
}

#[test]
fn nested_1() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 1;
                let b: i32 = 0;
                if a != 0 {
                    b = 1;
                }
                elif b != 0 {
                    b = 2;
                }
                return b;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn nested_2() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                let b: i32 = 1;
                if a != 0 {
                    b = 1;
                }
                elif b != 0 {
                    b = 2;
                }
                return b;
            }
        "##},
        "main",
        2,
    );
}

#[test]
fn nested_3() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                if true {
                    if true {
                        a = 3;
                    }
                    else {
                        a = 4;
                    }
                }
                return a;
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn nested_4() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                if true {
                    if false {
                        a = 3;
                    }
                    else {
                        a = 4;
                    }
                }
                return a;
            }
        "##},
        "main",
        4,
    );
}

#[test]
fn nested_5() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                if false {
                    if false {
                        a = 3;
                    }
                    else {
                        a = 4;
                    }
                }
                else {
                    a = 1;
                }
                return a;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn if_not_taken() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                let b: i32 = 2;
                if a != 0 {
                    b = 1;
                }
                return b;
            }
        "##},
        "main",
        2,
    );
}

#[test]
fn if_taken() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 1;
                let b: i32 = 0;
                if a != 0 {
                    b = 1;
                }
                return b;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn multiple_if() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                let b: i32 = 0;
                if a != 0 {
                    a = 2;
                }
                else {
                    a = 3;
                }
                if b != 0 {
                    b = 4;
                }
                else {
                    b = 5;
                }
                return a + b;
            }
        "##},
        "main",
        8,
    );
}

#[test]
fn empty_elif_with_else() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                if a != 0 {
                    a = 1;
                }
                elif true {
                }
                else {
                    a = 3;
                }
                return a;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn empty_elif_without_else() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                if a != 0 {
                    a = 1;
                }
                elif true {
                }
                return a;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn elif_all_paths_terminate() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                if false {
                    return 0;
                }
                elif true {
                    return 3;
                }
                else {
                    return 2;
                }
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn multi_elif_all_paths_terminate() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                if false {
                    return 0;
                }
                elif false {
                    return 3;
                }
                elif true {
                    return 7;
                }
                else {
                    return 2;
                }
            }
        "##},
        "main",
        7,
    );
}

#[test]
fn unreachable_after_if() {
    assert_compile_and_warning(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                if true {
                    return 1;
                }
                else {
                    return 3;
                }
                return 2;
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
fn i32_as_if_condition() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                if 8 {
                    return 1;
                }
                else {
                    return 3;
                }
            }
        "##},
        SourceLocation {
            index: 30,
            line: 2,
            column: 7,
        },
    );
}

#[test]
fn i32_as_elif_condition() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                if false {
                    return 1;
                }
                elif 69 {
                    return 2;
                }
                else {
                    return 3;
                }
            }
        "##},
        SourceLocation {
            index: 71,
            line: 5,
            column: 9,
        },
    );
}
