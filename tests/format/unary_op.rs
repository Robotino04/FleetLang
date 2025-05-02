use indoc::indoc;

use crate::common::assert_formatting_and_same_behaviour;

#[test]
fn collapse_minus() {
    assert_formatting_and_same_behaviour::<i32>(
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
        "a",
    );
}

#[test]
fn expand_minus() {
    assert_formatting_and_same_behaviour::<i32>(
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
        "a",
    );
}

#[test]
fn collapse_binary_not() {
    assert_formatting_and_same_behaviour::<i32>(
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
        "a",
    );
}

#[test]
fn expand_binary_not() {
    assert_formatting_and_same_behaviour::<i32>(
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
        "a",
    );
}

#[test]
fn collapse_logical_not() {
    assert_formatting_and_same_behaviour::<i32>(
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
        "a",
    );
}

#[test]
fn expand_logical_not() {
    assert_formatting_and_same_behaviour::<i32>(
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
        "a",
    );
}

#[test]
fn collapse_mixed() {
    assert_formatting_and_same_behaviour::<i32>(
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
        "a",
    );
}

#[test]
fn expand_mixed() {
    assert_formatting_and_same_behaviour::<i32>(
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
        "a",
    );
}

#[test]
fn remove_paren_nested() {
    assert_formatting_and_same_behaviour::<i32>(
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
        "a",
    );
}
