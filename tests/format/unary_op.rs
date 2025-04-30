use indoc::indoc;

use crate::common::assert_formatting;

#[test]
fn collapse_minus() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return -   0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return -0;
            }"##
        },
    );
}

#[test]
fn expand_minus() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return-0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return -0;
            }"##
        },
    );
}

#[test]
fn collapse_binary_not() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return ~   0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return ~0;
            }"##
        },
    );
}

#[test]
fn expand_binary_not() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return~0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return ~0;
            }"##
        },
    );
}

#[test]
fn collapse_logical_not() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return !   0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return !0;
            }"##
        },
    );
}

#[test]
fn expand_logical_not() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return!0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return !0;
            }"##
        },
    );
}

#[test]
fn collapse_mixed() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return ! ~    -  0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return !~-0;
            }"##
        },
    );
}

#[test]
fn expand_mixed() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return!-~0;
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return !-~0;
            }"##
        },
    );
}

#[test]
fn remove_paren_nested() {
    assert_formatting(
        indoc! {r##"
            let a = () -> i32 {
                return ~(-(5));
            }"##
        },
        indoc! {r##"
            let a = () -> i32 {
                return ~-5;
            }"##
        },
    );
}
