use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::{assert_compile_and_return_value, assert_compile_and_warning};

#[test]
fn if_else() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                if a {
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
                if a {
                    b = 1;
                }
                elif b {
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
                if a {
                    b = 1;
                }
                elif b {
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
                if 1 {
                    if 2 {
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
                if 1 {
                    if 0 {
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
                if 0 {
                    if 0 {
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
                if a {
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
                if a {
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
                if a {
                    a = 2;
                }
                else {
                    a = 3;
                }
                if b {
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
                if a {
                    a = 1;
                }
                elif 1 {
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
                if a {
                    a = 1;
                }
                elif 1 {
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
                if 0 {
                    return 0;
                }
                elif 2 {
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
                if 0 {
                    return 0;
                }
                elif 0 {
                    return 3;
                }
                elif 3 {
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
                if 1 {
                    return 1;
                }
                else {
                    return 3;
                }
                return 2;
            }
        "##},
        SourceLocation {
            index: 117,
            line: 9,
            column: 4,
        },
    );
}
