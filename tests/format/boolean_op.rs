use indoc::indoc;

use crate::common::assert_formatting;

#[test]
fn expand_eq() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0==0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 == 0;
            }"##
        },
    );
}

#[test]
fn collapse_eq() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0   ==   0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 == 0;
            }"##
        },
    );
}

#[test]
fn expand_ge() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0>=0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 >= 0;
            }"##
        },
    );
}

#[test]
fn collapse_qe() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0   >=   0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 >= 0;
            }"##
        },
    );
}

#[test]
fn expand_gt() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0>0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 > 0;
            }"##
        },
    );
}

#[test]
fn collapse_qt() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0   >   0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 > 0;
            }"##
        },
    );
}

#[test]
fn expand_le() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0<=0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 <= 0;
            }"##
        },
    );
}

#[test]
fn collapse_le() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0   <=   0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 <= 0;
            }"##
        },
    );
}

#[test]
fn expand_lt() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0<0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 < 0;
            }"##
        },
    );
}

#[test]
fn collapse_lt() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0   <   0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 < 0;
            }"##
        },
    );
}

#[test]
fn expand_ne() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0!=0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 != 0;
            }"##
        },
    );
}

#[test]
fn collapse_ne() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0   !=   0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 != 0;
            }"##
        },
    );
}

#[test]
fn expand_and() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0&&0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 && 0;
            }"##
        },
    );
}

#[test]
fn collapse_and() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0   &&   0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 && 0;
            }"##
        },
    );
}

#[test]
fn expand_or() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0||0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 || 0;
            }"##
        },
    );
}

#[test]
fn collapse_or() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 0   ||   0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 || 0;
            }"##
        },
    );
}

#[test]
fn remove_parens_1() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return (0 && 3) || 7;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0 && 3 || 7;
            }"##
        },
    );
}

#[test]
fn remove_parens_2() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return (3 + 2) <= (9 || 2);
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 3 + 2 <= (9 || 2);
            }"##
        },
    );
}

#[test]
fn remove_parens_3() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 3 || (1 == 7 - 8) && (2 != 9);
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 3 || 1 == 7 - 8 && 2 != 9;
            }"##
        },
    );
}
