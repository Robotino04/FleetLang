use indoc::indoc;

use crate::common::assert_formatting_and_same_behaviour;

#[test]
fn collapse_while() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                while     false    
                {
                                1;
                }

                return 2;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                while false {
                    1;
                }

                return 2;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_while() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                while false{1;}
                return 2;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                while false {
                    1;
                }
                return 2;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_for() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                for
                (   let    i :
                i32    =

                0    ;    i  <  
                        10
                ;    i  =  i 
                +  1  )
                {
                    5   ;
                }
                return 2;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    5;
                }
                return 2;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_for() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                for(let i:i32=0;i<10;i=i+1){5;}
                return 2;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    5;
                }
                return 2;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_skip() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    skip  ;
                }
                return 2;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    skip;
                }
                return 2;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_break() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    break  ;
                }
                return 2;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    break;
                }
                return 2;
            }"##
        },
        "main",
    );
}

#[test]
fn for_remove_parens_condition() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 0; ((( ( i ) ) < ( 10 ))); i = i + 1) {
                    break;
                }
                return 2;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    break;
                }
                return 2;
            }"##
        },
        "main",
    );
}

#[test]
fn for_remove_parens_incrementer() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 0; i < 10; ( i = (( ( i ) ) + ( 1 )) )) {
                    break;
                }
                return 2;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    break;
                }
                return 2;
            }"##
        },
        "main",
    );
}

#[test]
fn preserve_spacing_skip() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    skip;

                    // preserve pls
                }
                return 2;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    skip;

                    // preserve pls
                }
                return 2;
            }"##
        },
        "main",
    );
}

#[test]
fn preserve_spacing_break() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    break;

                    // preserve pls
                }
                return 2;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    break;

                    // preserve pls
                }
                return 2;
            }"##
        },
        "main",
    );
}
