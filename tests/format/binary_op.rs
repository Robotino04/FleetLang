use indoc::indoc;

use crate::common::assert_formatting;

#[test]
fn collapse_add() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 12   +
                34;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 12 + 34;
            }"##
        },
    );
}

#[test]
fn expand_add() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 12+34;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 12 + 34;
            }"##
        },
    );
}

#[test]
fn collapse_sub() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 12   -
                34;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 12 - 34;
            }"##
        },
    );
}

#[test]
fn expand_sub() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 12-34;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 12 - 34;
            }"##
        },
    );
}

#[test]
fn collapse_mul() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 12   *
                34;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 12 * 34;
            }"##
        },
    );
}

#[test]
fn expand_mul() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 12*34;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 12 * 34;
            }"##
        },
    );
}

#[test]
fn collapse_div() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 12   /
                34;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 12 / 34;
            }"##
        },
    );
}

#[test]
fn expand_div() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 12/34;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 12 / 34;
            }"##
        },
    );
}

#[test]
fn collapse_mod() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 12   %
                34;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 12 % 34;
            }"##
        },
    );
}

#[test]
fn expand_mod() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 12%34;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 12 % 34;
            }"##
        },
    );
}

#[test]
fn collapse_paren() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return    1   *    
                (2   -      4)    ;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 1 * (2 - 4);
            }"##
        },
    );
}

#[test]
fn collapse_nested_paren() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return !  (  (  1  +  2   )    *   4 )  ;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return !((1 + 2) * 4);
            }"##
        },
    );
}

#[test]
fn expand_paren() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 1*(2-4);
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 1 * (2 - 4);
            }"##
        },
    );
}

#[test]
fn remove_paren_lower_inner_precedence() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return (1 * 2) - 4;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 1 * 2 - 4;
            }"##
        },
    );
}

#[test]
fn remove_paren_higher_inner_precedence() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return (1 + 2) % 4;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return (1 + 2) % 4;
            }"##
        },
    );
}

#[test]
fn remove_paren_top() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return (1 * 2);
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 1 * 2;
            }"##
        },
    );
}

#[test]
fn remove_paren_bottom() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return 1 + (1);
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 1 + 1;
            }"##
        },
    );
}

#[test]
fn remove_paren_only_one() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return (1) * (2 - 4);
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 1 * (2 - 4);
            }"##
        },
    );
}

#[test]
fn remove_paren_nested_simple() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return (((((99)))));
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 99;
            }"##
        },
    );
}

#[test]
fn remove_paren_nested_complex() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return (((5 + (5 * 2)) + 3)/2);
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return (5 + 5 * 2 + 3) / 2;
            }"##
        },
    );
}
