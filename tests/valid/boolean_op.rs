use indoc::indoc;

use crate::common::assert_compile_and_return_value;

#[test]
fn eq_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 1 == 2;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn eq_true() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 1 == 1;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn ge_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 1 >= 2;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn ge_true() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 1 >= 1;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn gt_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 1 > 2;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn gt_true() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 1 > 0;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn le_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 1 <= -1;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn le_true() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 0 <= 2;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn lt_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 2 < 1;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn ne_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 0 != 0;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn ne_true() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return -1 != -2;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn and_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 1 && 0;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn and_true() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 1 && -1;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn or_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 0 || 0;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn or_true() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 1 || 0;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn precedence_1() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 1 || 0 && 2;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn precedence_2() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return (1 || 0) && 0;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn precedence_3() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 2 == 2 > 0;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn precedence_4() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 2 == 2 || 0;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn and_short_circuit() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                let b: i32 = 0;
                a && (b = 5);
                return b;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn or_short_circuit() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 1;
                let b: i32 = 0;
                a || (b = 5);
                return b;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn multi_short_circuit() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                a || (a = 3) || (a = 4);
                return a;
            }
        "##},
        "main",
        3,
    );
}
