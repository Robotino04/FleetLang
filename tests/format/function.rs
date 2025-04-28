use indoc::indoc;

use crate::common::assert_formatting;


#[test]
fn empty_file() {
    assert_formatting("", "");
}

#[test]
fn expand() {
    assert_formatting(
        indoc! {r##"
                let a=()->i32{return 0;}
            "##},
        indoc! {r##"
                let a = () -> i32 {
                    return 0;
                }"##
        },
    );
}

#[test]
fn collapse() {
    assert_formatting(
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
            "##},
        indoc! {r##"
                let a = () -> i32 {
                    return 0;
                }"##
        },
    );
}

#[test]
fn expand_2() {
    assert_formatting(
        indoc! {r##"
                let a=()->i32{return 0;}let b=()->i32{return 0;}
            "##},
        indoc! {r##"
                let a = () -> i32 {
                    return 0;
                }
                let b = () -> i32 {
                    return 0;
                }"##
        },
    );
}

#[test]
fn collapse_2() {
    assert_formatting(
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
                ;}
            "##},
        indoc! {r##"
                let a = () -> i32 {
                    return 0;
                }
                let b = () -> i32 {
                    return 0;
                }"##
        },
    );
}
