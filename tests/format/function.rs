use indoc::indoc;

use crate::common::{assert_formatting, assert_formatting_and_same_behaviour};

#[test]
fn empty_file() {
    assert_formatting("", "");
}

#[test]
fn expand() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let a=()->i32{return 0;}
            "##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0;
            }"##
        },
        "a",
    );
}

#[test]
fn collapse() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let     a

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
            let a = () -> i32 {
                return 0;
            }"##
        },
        "a",
    );
}

#[test]
fn expand_2() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let a=()->i32{return 0;}let b=()->i32{return 0;}
            "##
        },
        indoc! {r##"
            let a = () -> i32 {
                return 0;
            }
            let b = () -> i32 {
                return 0;
            }"##
        },
        "a",
    );
}

#[test]
fn collapse_2() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let         a=
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
            let a = () -> i32 {
                return 0;
            }
            let b = () -> i32 {
                return 0;
            }"##
        },
        "a",
    );
}
