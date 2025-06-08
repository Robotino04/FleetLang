use indoc::indoc;

use crate::common::assert_formatting_and_same_behaviour;

#[test]
fn expand() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main=()->i32{return 0;}
            "##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let     main

            = 
            (    
                )

            ->   i32
              {
            return
                    0
            ;
            }
            "##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_2() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main=()->i32{return 0;}let b=()->i32{return 0;}
            "##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 0;
            }
            let b = () -> i32 {
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_2() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let         main=
            ()
              ->
            i32
                {
                return
            0;}

            let
            b =

            (    )
            ->  i32 {
            return 0
            ;}  "##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 0;
            }
            let b = () -> i32 {
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_multiple_statements() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                4 + 5;4 + 5;4 + 5;return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                4 + 5;
                4 + 5;
                4 + 5;
                return 0;
            }"##
        },
        "main",
    );
}

#[ignore = "Not sure how to do this yet"]
#[test]
fn preserve_newlines_without_comments() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                4 + 5;
                4 + 5;

                4 + 5;
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                4 + 5;
                4 + 5;

                4 + 5;
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_parens_multiple_statements() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                (1 * 2);
                (1 + 1);
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                1 * 2;
                1 + 1;
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_parens_multiple_returns() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return (1 * 2);
            }
            let main2 = () -> i32 {
                return (1 + 1);
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return 1 * 2;
            }
            let main2 = () -> i32 {
                return 1 + 1;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_parameters() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return foo(2, 3);
            }
            let foo = (a:i32,b:i32) -> i32 {
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return foo(2, 3);
            }
            let foo = (a: i32, b: i32) -> i32 {
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_parameters() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return foo(2, 3);
            }
            let foo = (a  :   i32  ,  b
            :  i32 ) -> i32 {
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return foo(2, 3);
            }
            let foo = (a: i32, b: i32) -> i32 {
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_arguments() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return foo(2,3);
            }
            let foo = (a:i32, b:i32) -> i32 {
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return foo(2, 3);
            }
            let foo = (a: i32, b: i32) -> i32 {
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_arguments() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return foo
                (   2  , 3   );
            }
            let foo = (a:i32, b:i32) -> i32 {
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return foo(2, 3);
            }
            let foo = (a: i32, b: i32) -> i32 {
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_parens_arguments() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return foo(((2)), ((3 + 2) * 4) * 2);
            }
            let foo = (a:i32, b:i32) -> i32 {
                return 0;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return foo(2, (3 + 2) * 4 * 2);
            }
            let foo = (a: i32, b: i32) -> i32 {
                return 0;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_trailing_comma() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                return foo(foo(1, 2 /* a */,/* b */), 4 /* c */,/* d */);
            }
            let foo = (a: i32, b: i32) -> i32 {
                return a + b;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                return foo(foo(1, 2 /* a */ /* b */), 4 /* c */ /* d */);
            }
            let foo = (a: i32, b: i32) -> i32 {
                return a + b;
            }"##
        },
        "main",
    );
}
