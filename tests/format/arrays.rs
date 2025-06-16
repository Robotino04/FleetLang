use indoc::indoc;

use crate::common::{assert_formatting, assert_formatting_and_same_behaviour};

#[test]
fn expand_literal() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                [1,2,3,4,5];
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                [1, 2, 3, 4, 5];
                return 0;
            }"##
        },
        "main",
    );
}
#[test]
fn collapse_literal() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                [
                1   , 2   ,
                    3 ,4  ,   5
                ]   ;
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                [1, 2, 3, 4, 5];
                return 0;
            }"##
        },
        "main",
    );
}
#[test]
fn collapse_lvalue() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3, 4, 5];
                a  [ 
                2  ]   = 123;
                return a[2];
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3, 4, 5];
                a[2] = 123;
                return a[2];
            }"##
        },
        "main",
    );
}
#[test]
fn collapse_index() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3, 4, 5];
                return   a
                [  2  ] ;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3, 4, 5];
                return a[2];
            }"##
        },
        "main",
    );
}
#[test]
fn collapse_type_unsized() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a  :  i32
                [

                        ]  = [1, 2, 3, 4, 5];
                return a[2];
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a: i32[] = [1, 2, 3, 4, 5];
                return a[2];
            }"##
        },
        "main",
    );
}
#[test]
fn collapse_type_sized() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a  :  i32
                [
            5
                        ]  = [1, 2, 3, 4, 5];
                return a[2];
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a: i32[5] = [1, 2, 3, 4, 5];
                return a[2];
            }"##
        },
        "main",
    );
}
#[test]
fn remove_parens_literal() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a = [((1 + 2) * 3 ) * 7, ((9 / 2) > 3) as i32, 3, 4, 5];
                return a[2];
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a = [(1 + 2) * 3 * 7, (9 / 2 > 3) as i32, 3, 4, 5];
                return a[2];
            }"##
        },
        "main",
    );
}
#[test]
fn remove_parens_index() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3, 4, 5];
                return a[((((33 / 2) + 9) > 2) as i32)];
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3, 4, 5];
                return a[(33 / 2 + 9 > 2) as i32];
            }"##
        },
        "main",
    );
}
#[test]
fn remove_parens_lvalue() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3, 4, 5];
                a[((((33 / 2) + 9) > 2) as i32)] = 99;
                return a[1];
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3, 4, 5];
                a[(33 / 2 + 9 > 2) as i32] = 99;
                return a[1];
            }"##
        },
        "main",
    );
}
#[test]
fn remove_parens_lvalue_2d() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a = [[1, 2, 3, 4, 5], [5, 4, 3, 2, 1]];
                (a[0])[2] = 99;
                return a[0][2];
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a = [[1, 2, 3, 4, 5], [5, 4, 3, 2, 1]];
                a[0][2] = 99;
                return a[0][2];
            }"##
        },
        "main",
    );
}
#[test]
fn remove_parens_index_2d() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let a = [[1, 2, 3, 4, 5], [5, 4, 3, 2, 1]];
                return (a[0])[2];
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let a = [[1, 2, 3, 4, 5], [5, 4, 3, 2, 1]];
                return a[0][2];
            }"##
        },
        "main",
    );
}
