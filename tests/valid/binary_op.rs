use indoc::indoc;

use crate::common::{assert_compile_and_return_value, assert_compile_and_return_value_unformatted};

#[test]
fn add_1_2() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 1 + 2;
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn add_unary() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return ~2 + 3;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn sub_1_2() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 1 - 2;
            }
        "##},
        "main",
        -1,
    );
}

#[test]
fn sub_2_minus1() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> i32 {
                return 2 - -1;
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn sub_associativity() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 1 - 2 - 3;
            }
        "##},
        "main",
        -4,
    );
}

#[test]
fn mul_2_3() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 2 * 3;
            }
        "##},
        "main",
        6,
    );
}

#[test]
fn mul_minus2_3() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> i32 {
                return (-2) * 3;
            }
        "##},
        "main",
        -6,
    );
}

#[test]
fn mul_2_minus3() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> i32 {
                return 2 * (-3);
            }
        "##},
        "main",
        -6,
    );
}

#[test]
fn mul_minus2_minus3() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> i32 {
                return (-2) * (-3);
            }
        "##},
        "main",
        6,
    );
}

#[test]
fn div_4_2() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 4 / 2;
            }
        "##},
        "main",
        2,
    );
}

#[test]
fn div_negative1() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> i32 {
                return (-12) / 5;
            }
        "##},
        "main",
        -2,
    );
}

#[test]
fn div_negative2() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> i32 {
                return 12 / (-5);
            }
        "##},
        "main",
        -2,
    );
}

#[test]
fn div_associativity() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 6 / 3 / 2;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn mod_6_5() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 6 % 5;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn mod_negative1() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> i32 {
                return (-12) % 7;
            }
        "##},
        "main",
        -5,
    );
}

#[test]
fn mod_negative2() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> i32 {
                return 12 % (-7);
            }
        "##},
        "main",
        5,
    );
}

#[test]
fn mod_negative3() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> i32 {
                return (-12) % (-7);
            }
        "##},
        "main",
        -5,
    );
}

#[test]
fn parens_1() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return (2 * 3 + 3) * 2 - 1;
            }
        "##},
        "main",
        17,
    );
}

#[test]
fn parens_2() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 2 * (3 + 4);
            }
        "##},
        "main",
        14,
    );
}

#[test]
fn precedence() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 2 + 3 * 4;
            }
        "##},
        "main",
        14,
    );
}

#[test]
fn unary_parens() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return ~(1 + 1);
            }
        "##},
        "main",
        -3,
    );
}
