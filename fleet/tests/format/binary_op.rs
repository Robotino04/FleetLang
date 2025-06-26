use indoc::indoc;

use crate::common::assert_formatting_and_same_behaviour;

#[test]
fn collapse_add() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 12   +
                34;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 12 + 34;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_add() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 12+34;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 12 + 34;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_sub() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 12   -
                34;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 12 - 34;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_sub() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 12-34;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 12 - 34;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_mul() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 12   *
                34;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 12 * 34;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_mul() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 12*34;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 12 * 34;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_div() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 12   /
                34;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 12 / 34;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_div() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 12/34;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 12 / 34;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_mod() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 12   %
                34;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 12 % 34;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_mod() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 12%34;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 12 % 34;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_paren() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return    1   *    
                (2   -      4)    ;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 1 * (2 - 4);
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_nested_paren() {
    assert_formatting_and_same_behaviour::<bool>(
        indoc! {r##"
            let main = () -> bool {
                return !  (  (  1  +  2   )    *   4 )  ;
            }"##
        },
        indoc! {r##"
            let main = () -> bool {
                return !((1 + 2) * 4);
            }"##
        },
        "main",
    );
}

#[test]
fn expand_paren() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 1*(2-4);
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 1 * (2 - 4);
            }"##
        },
        "main",
    );
}

#[test]
fn remove_paren_lower_inner_precedence() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return (1 * 2) - 4;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 1 * 2 - 4;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_paren_higher_inner_precedence() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return (1 + 2) % 4;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return (1 + 2) % 4;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_paren_top() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return (1 * 2);
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 1 * 2;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_paren_bottom() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 1 + (1);
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 1 + 1;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_paren_only_one() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return (1) * (2 - 4);
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 1 * (2 - 4);
            }"##
        },
        "main",
    );
}

#[test]
fn remove_paren_nested_simple() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return (((((99)))));
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 99;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_paren_nested_complex() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return (((5 + (5 * 2)) + 3)/2);
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return (5 + 5 * 2 + 3) / 2;
            }"##
        },
        "main",
    );
}

#[test]
fn keep_paren_associativity_sub() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 1 - (2 - 3);
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 1 - (2 - 3);
            }"##
        },
        "main",
    );
}

#[test]
fn remove_paren_associativity_sub() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return (1 - 2) - 3;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 1 - 2 - 3;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_paren_associativity_add_left() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return (1 + 2) + 3;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 1 + 2 + 3;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_paren_associativity_add_right() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 1 + (2 + 3);
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 1 + 2 + 3;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_paren_associativity_add_sub() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 1 + (2 - 3);
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 1 + 2 - 3;
            }"##
        },
        "main",
    );
}

#[test]
fn keep_paren_associativity_nested() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 1 - (((2 - 3)));
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 1 - (2 - 3);
            }"##
        },
        "main",
    );
}

#[test]
fn keep_and_remove_paren_associativity_nested() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return ((1 - (((2 - 3)))) + 7) - 3;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 1 - (2 - 3) + 7 - 3;
            }"##
        },
        "main",
    );
}
