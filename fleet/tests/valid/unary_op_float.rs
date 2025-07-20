use indoc::indoc;

use crate::common::assert_compile_and_return_value;


#[test]
fn negative_5() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> f32 {
                return -5.0;
            }
        "##},
        "main",
        -5.0f32,
    );
}

#[test]
fn nested_1() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> bool {
                return !-3.0;
            }
        "##},
        "main",
        false,
    );
}
