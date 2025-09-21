use indoc::indoc;

use crate::common::assert_formatting_and_same_behaviour;

#[test]
fn comma_before_comment() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let s: struct {
                    a: i32 // Hello! My name is `a`.
                } = idk {
                    a: 5 // and I have the value 5
                };

                return s.a;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let s: struct {
                    a: i32, // Hello! My name is `a`.
                } = idk {
                    a: 5, // and I have the value 5
                };

                return s.a;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_struct() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let 
                s
                =
                struct 
                {
                a
                :
                i32
                ,
                b
                :
                f32,
                }
                {
                a
                :
                33
                ,
                b
                :
                3.1415
                ,
                }
                ;

                return s.a;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let s = struct {
                    a: i32,
                    b: f32,
                } {
                    a: 33,
                    b: 3.1415,
                };

                return s.a;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_struct() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                let s=struct{a:i32,b:f32}{a:33,b:3.1415};

                return s.a;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                let s = struct {
                    a: i32,
                    b: f32,
                } {
                    a: 33,
                    b: 3.1415,
                };

                return s.a;
            }"##
        },
        "main",
    );
}
