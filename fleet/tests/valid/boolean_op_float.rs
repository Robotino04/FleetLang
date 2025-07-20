use indoc::indoc;

use crate::common::assert_compile_and_return_value;

#[test]
fn eq_false() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return 1.0 == 2.0;
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
                return 1.0 == 1.0;
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
                return 1.0 >= 2.0;
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
                return 1.0 >= 1.0;
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
                return 1.0 > 2.0;
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
                return 1.0 > 0.0;
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
                return 1.0 <= -1.0;
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
                return 0.0 <= 2.0;
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
                return 2.0 < 1.0;
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
                return 0.0 != 0.0;
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
                return -1.0 != -2.0;
            }
        "##},
        "main",
        true,
    );
}

#[test]
fn precedence_3() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return true == 3.0 >= 2.0;
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
                return 2.0 == 2.0 || false;
            }
        "##},
        "main",
        true,
    );
}
