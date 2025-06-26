use indoc::indoc;

use crate::common::assert_compile_and_return_value;

#[test]
fn eq_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return 1 == 2;
            }
        "##},
        "main",
        false,
    );
}

#[test]
fn eq_true() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return 1 == 1;
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn ge_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return 1 >= 2;
            }
        "##},
        "main",
        false,
    );
}

#[test]
fn ge_true() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return 1 >= 1;
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn gt_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return 1 > 2;
            }
        "##},
        "main",
        false,
    );
}

#[test]
fn gt_true() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return 1 > 0;
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn le_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return 1 <= -1;
            }
        "##},
        "main",
        false,
    );
}

#[test]
fn le_true() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return 0 <= 2;
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn lt_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return 2 < 1;
            }
        "##},
        "main",
        false,
    );
}

#[test]
fn ne_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return 0 != 0;
            }
        "##},
        "main",
        false,
    );
}

#[test]
fn ne_true() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return -1 != -2;
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn and_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return true && false;
            }
        "##},
        "main",
        false,
    );
}

#[test]
fn and_true() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return true && true;
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn or_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return false || false;
            }
        "##},
        "main",
        false,
    );
}

#[test]
fn or_true() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return true || false;
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn precedence_1() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return true || false && true;
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn precedence_2() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return (true || false) && false;
            }
        "##},
        "main",
        false,
    );
}

#[test]
fn precedence_3() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return true == 3 >= 2;
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn precedence_4() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return 2 == 2 || false;
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn and_short_circuit() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: bool = false;
                let b: i32 = 0;
                a && (b = 5) as bool;
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
                let a: bool = true;
                let b: i32 = 0;
                a || (b = 5) as bool;
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
                a as bool || (a = 3) as bool || (a = 4) as bool;
                return a;
            }
        "##},
        "main",
        3,
    );
}
