use indoc::indoc;

use crate::common::{assert_compile_and_return_value, assert_compile_and_return_value_unformatted};

#[test]
fn add_1_2() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return 1.0 + 2.0;
            }
        "##},
        "main",
        3.0f32,
    );
}

#[test]
fn add_unary() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return -2.0 + 3.0;
            }
        "##},
        "main",
        1.0f32,
    );
}

#[test]
fn sub_1_2() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return 1.0 - 2.0;
            }
        "##},
        "main",
        -1.0f32,
    );
}

#[test]
fn sub_2_minus1() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> f32 {
                return 2.0 - -1.0;
            }
        "##},
        "main",
        3.0f32,
    );
}

#[test]
fn sub_associativity() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return 1.0 - 2.0 - 3.0;
            }
        "##},
        "main",
        -4.0f32,
    );
}

#[test]
fn mul_2_3() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return 2.0 * 3.0;
            }
        "##},
        "main",
        6.0f32,
    );
}

#[test]
fn mul_minus2_3() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> f32 {
                return (-2.0) * 3.0;
            }
        "##},
        "main",
        -6.0f32,
    );
}

#[test]
fn mul_2_minus3() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> f32 {
                return 2.0 * (-3.0);
            }
        "##},
        "main",
        -6.0f32,
    );
}

#[test]
fn mul_minus2_minus3() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> f32 {
                return (-2.0) * (-3.0);
            }
        "##},
        "main",
        6.0f32,
    );
}

#[test]
fn div_4_2() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return 4.0 / 2.0;
            }
        "##},
        "main",
        2.0f32,
    );
}

#[test]
fn div_negative1() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> f32 {
                return (-12.0) / 5.0;
            }
        "##},
        "main",
        -2.4f32,
    );
}

#[test]
fn div_negative2() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> f32 {
                return 12.0 / (-5.0);
            }
        "##},
        "main",
        -2.4f32,
    );
}

#[test]
fn div_associativity() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return 6.0 / 3.0 / 2.0;
            }
        "##},
        "main",
        1.0f32,
    );
}

#[test]
fn mod_6_5() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return 6.0 % 5.0;
            }
        "##},
        "main",
        1.0f32,
    );
}

#[test]
fn mod_negative1() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> f32 {
                return (-12.0) % 7.0;
            }
        "##},
        "main",
        -5.0f32,
    );
}

#[test]
fn mod_negative2() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> f32 {
                return 12.0 % (-7.0);
            }
        "##},
        "main",
        5.0f32,
    );
}

#[test]
fn mod_negative3() {
    assert_compile_and_return_value_unformatted(
        indoc! {r##"
            let main = () -> f32 {
                return (-12.0) % (-7.0);
            }
        "##},
        "main",
        -5.0f32,
    );
}

#[test]
fn parens_1() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return (2.0 * 3.0 + 3.0) * 2.0 - 1.0;
            }
        "##},
        "main",
        17.0f32,
    );
}

#[test]
fn parens_2() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return 2.0 * (3.0 + 4.0);
            }
        "##},
        "main",
        14.0f32,
    );
}

#[test]
fn precedence() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return 2.0 + 3.0 * 4.0;
            }
        "##},
        "main",
        14.0f32,
    );
}

#[test]
fn unary_parens() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return -(1.0 + 1.0);
            }
        "##},
        "main",
        -2.0f32,
    );
}

#[test]
fn unused_exp() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                2.0 + 2.0;
                return 0.0;
            }
        "##},
        "main",
        0.0f32,
    );
}
