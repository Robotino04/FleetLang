use indoc::indoc;

use crate::common::assert_formatting_and_same_behaviour;

#[test]
fn expand() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a:i32=0;
                return a;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                return a;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let    a :
                i32  =  0;
                return a;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                return a;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_parens_multi_assign() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                let b: i32 = 0;
                let c: i32 = 0;
                (a = (b = (c = 3)));
                return a + b + c;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                let b: i32 = 0;
                let c: i32 = 0;
                a = b = c = 3;
                return a + b + c;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_parens_initializer() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = ((5 / 2) && 9);
                return a;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 5 / 2 && 9;
                return a;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_parens_assignment() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                a = ((5 / 2) && 9);
                return a;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                a = 5 / 2 && 9;
                return a;
            }"##
        },
        "main",
    );
}

#[test]
fn keep_params_assign_expr() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                return (a = 2) + 3;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                return (a = 2) + 3;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_parens_multiple_definition() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = (2 + 2 * 2);
                let b: i32 = (2 + 2 * 2);
                return a;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 2 + 2 * 2;
                let b: i32 = 2 + 2 * 2;
                return a;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_parens_multiple_assignments() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = (2 + 2 * 2);
                let b: i32 = (2 + 2 * 2);
                return a;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 2 + 2 * 2;
                let b: i32 = 2 + 2 * 2;
                return a;
            }"##
        },
        "main",
    );
}
