use indoc::indoc;

use crate::common::assert_formatting_and_same_behaviour;

#[test]
fn remove_parens_cast() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return ((9 / 2) > 3) as i32;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return (9 / 2 > 3) as i32;
            }"##
        },
        "main",
    );
}
#[test]
fn remove_parens_cast_associativity() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return (((9 / 2) > 3) as i16) as i32;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return (9 / 2 > 3) as i16 as i32;
            }"##
        },
        "main",
    );
}
#[test]
fn remove_parens_cast_unary() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return (!2) as i32;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return !2 as i32;
            }"##
        },
        "main",
    );
}
