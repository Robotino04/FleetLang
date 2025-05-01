use indoc::indoc;

use crate::common::assert_compile_and_return_value;

#[test]
fn block_comment() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                /*return 5;*/
                return 2;
            }
        "##},
        "main",
        2,
    );
}

#[test]
fn line_comment() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                // return 5;
                return 3;
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn big_block_comment() {
    assert_compile_and_return_value(
        indoc! {r##"
            /*
            let main = () -> i32 {
                return 7;
            }
            */

            let main = () -> i32 {
                return -13;
            }
        "##},
        "main",
        -13,
    );
}
