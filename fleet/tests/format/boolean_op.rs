use indoc::indoc;

use crate::common::{assert_formatting, assert_formatting_and_same_behaviour};

#[test]
fn expand_eq() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return 0==0;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return 0 == 0;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_eq() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return 0   ==   0;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return 0 == 0;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_ge() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return 0>=0;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return 0 >= 0;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_qe() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return 0   >=   0;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return 0 >= 0;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_gt() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return 0>0;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return 0 > 0;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_qt() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return 0   >   0;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return 0 > 0;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_le() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return 0<=0;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return 0 <= 0;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_le() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return 0   <=   0;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return 0 <= 0;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_lt() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return 0<0;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return 0 < 0;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_lt() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return 0   <   0;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return 0 < 0;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_ne() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return 0!=0;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return 0 != 0;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_ne() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return 0   !=   0;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return 0 != 0;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_and() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return false&&false;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return false && false;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_and() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return false   &&   false;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return false && false;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_or() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return false||false;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return false || false;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_or() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return false   ||   false;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return false || false;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_parens_1() {
    assert_formatting(
        indoc! {r##"
            let main = () -> bool {
                return (0 && 3) || 7;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return 0 && 3 || 7;
            }"##
        },
    );
}

#[test]
fn remove_parens_2() {
    assert_formatting(
        indoc! {r##"
            let main = () -> bool {
                return (3 + 2) <= (9 || 2);
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return 3 + 2 <= (9 || 2);
            }"##
        },
    );
}

#[test]
fn remove_parens_3() {
    assert_formatting(
        indoc! {r##"
            let main = () -> i32 {
                return 3 || (1 == 7 - 8) && (2 != 9);
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 3 || 1 == 7 - 8 && 2 != 9;
            }"##
        },
    );
}

#[test]
fn preserve_spacing_expressions() {
    assert_formatting(
        indoc! {r##"
            let main = () -> i32 {
                3 || 1 == 7 - 8 && 2 != 9;

                return 3 || 1 == 7 - 8 && 2 != 9;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                3 || 1 == 7 - 8 && 2 != 9;

                return 3 || 1 == 7 - 8 && 2 != 9;
            }"##
        },
    );
}
