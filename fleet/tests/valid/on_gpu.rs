use indoc::indoc;

use crate::common::assert_compile_and_return_value;

#[test]
fn vector_addition() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3];
                let b = [4, 5, 6];
                let c = [0, 0, 0];
                on self.gpus[0][i = 3] (c[i]) {
                    c[i] = a[i] + b[i];
                }
                return c[0] + c[1] + c[2];
            }
        "##},
        "main",
        21,
    );
}
