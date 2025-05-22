use indoc::indoc;

use crate::common::assert_formatting_and_same_behaviour;

#[test]
fn complex_1() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let /* asd */ main = () -> i32 {
            /* asd */
                return /*a*/(/*b*/1 * 2/*c*/)/*d*/ + 2;   // test
                /* asd */
                    /* asd */
            // asd
            }// 213
                /*asd*/"##
        },
        indoc! {r##"
            let /* asd */ main = () -> i32 {
                /* asd */
                return /*a*/ /*b*/ 1 * 2 /*c*/ /*d*/ + 2; // test
                /* asd */
                /* asd */
                // asd
            } // 213
            /*asd*/"##
        },
        "main",
    );
}

#[test]
fn expand_multiple_comments() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                /*asd*//*Hello, World!*/// Bye
                return 5;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                /*asd*/ /*Hello, World!*/ // Bye
                return 5;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_multiple_comments() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                /*asd*/    /*Hello, World!*/      // Bye
                return 5;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                /*asd*/ /*Hello, World!*/ // Bye
                return 5;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_function_parameter() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = (   /*asdasd*/  /*abcd*/ ) -> i32 {
                return 4;
            }"##
        },
        indoc! {r##"
            let main = (/*asdasd*/ /*abcd*/) -> i32 {
                return 4;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_function_parameter() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = (/*1234*//*abcd*/) -> i32 {
                return 4;
            }"##
        },
        indoc! {r##"
            let main = (/*1234*/ /*abcd*/) -> i32 {
                return 4;
            }"##
        },
        "main",
    );
}

#[test]
fn parentheses() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return /*a*/(/*b*/1/*c*/+/*d*/2/*e*/)/*f*/*/*g*/3;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return /*a*/ (/*b*/ 1 /*c*/ + /*d*/ 2 /*e*/) /*f*/ * /*g*/ 3;
            }"##
        },
        "main",
    );
}

#[test]
fn function_definition() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let/**/main/**/=/**/(/**/)/**/->/**/i32/**/{/**/
                return 0;
            }"##
        },
        indoc! {r##"
            let /**/ main /**/ = /**/ (/**/) /**/ -> /**/ i32 /**/ { /**/
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn open_brace() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                /*a*/{/*b*/
                /*c*/
                    return 0;
                }
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                /*a*/ { /*b*/
                    /*c*/
                    return 0;
                }
            }"##
        },
        "main",
    );
}

#[test]
fn close_brace() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                {
                    return 0;
                /*a*/}/*b*/
            /*c*/
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                {
                    return 0;
                    /*a*/
                } /*b*/
                /*c*/
            }"##
        },
        "main",
    );
}

#[test]
fn close_brace_and_trailing() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 0; // test
            /* asd */}"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 0; // test
                /* asd */
            }"##
        },
        "main",
    );
}

#[test]
fn expand_line_comment() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 0; //Hi :3
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 0; // Hi :3
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_line_comment() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return 0; //         Hi :3
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 0; // Hi :3
            }"##
        },
        "main",
    );
}

#[test]
fn align_block_comment() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                /* Line 1
                        Line 2
                    Line 3 */
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                /* Line 1
                    Line 2
                Line 3 */
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn dont_trim_single_line_block_comment() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                /* Line 1 */
                /*Line 2*/
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                /* Line 1 */
                /*Line 2*/
                return 0;
            }"##
        },
        "main",
    );
}
#[test]
fn dedent() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                   /*
                   a
                   */
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                /*
                a
                */
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn empty_line_in_block() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                /*
                a
                b

                c
                */
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                /*
                a
                b

                c
                */
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn empty_line_with_spaces_in_block() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                /*
                a
                b
                
                c
                */
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                /*
                a
                b

                c
                */
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn empty_line_only() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                /*

                */
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                /*

                */
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn empty_line_with_spaces_only() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                /*
                
                */
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                /*

                */
                return 0;
            }"##
        },
        "main",
    );
}
