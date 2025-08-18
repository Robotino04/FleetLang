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

#[test]
fn internal_lvalue_defined() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3];
                let b = [0, 0, 0];

                on self.gpus[0][i = 3] (b[i]) {
                    let c = 0;
                    c = 5;
                    b[i] = c + a[i];
                }

                return b[0] + b[1] + b[2];
            }
        "##},
        "main",
        21,
    );
}

#[test]
fn full_array_access() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3];
                let b = [0, 0, 0];

                on self.gpus[0][i = 3] (b) {
                    b[i] = a[i];
                }

                return b[0] + b[1] + b[2];
            }
        "##},
        "main",
        6,
    );
}

#[test]
fn internal_array_read() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3];
                let b = [0, 0, 0];

                on self.gpus[0][i = 3] (b[i]) {
                    let c = [4, 5, 6];
                    b[i] = c[i] + a[i];
                }

                return b[0] + b[1] + b[2];
            }
        "##},
        "main",
        21,
    );
}

#[test]
fn internal_array_write() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3];
                let b = [0, 0, 0];

                on self.gpus[0][i = 3] () {
                    let c = [4, 5, 6];
                    c[i] = b[i] + a[i];
                }

                return b[0] + b[1] + b[2];
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn nested_array_index() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3];
                let b = [0, 0, 0];
                let c = [4, 5, 6];

                // HINT: if you find yourself writing code like this, you did something wrong
                on self.gpus[0][i = 3] (b[c[i] = i], c[i]) {
                    b[c[i] = i] = a[i];
                }

                return b[0] + b[1] + b[2];
            }
        "##},
        "main",
        6,
    );
}

#[test]
fn complex_lvalue() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3];
                let b = [0, 0, 0];

                on self.gpus[0][i = 3] (b[i], b[(i + 1) % 3]) {
                    b[i] = a[i];
                    b[(i + 1) % 3] = b[i];
                }

                return b[0];
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn matrix_multiplication() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
                let b = [[11, 12, 13], [14, 15, 16], [17, 18, 19]];
                let c = [[0, 0, 0], [0, 0, 0], [0, 0, 0]];

                on self.gpus[0][i = 3][j = 3] (c[i][j]) {
                    c[i][j] = 0;
                    for (let k = 0; k < 3; k = k + 1) {
                        c[i][j] = c[i][j] + a[i][k] * b[k][j];
                    }
                }

                return c[1][1];
            }
        "##},
        "main",
        231,
    );
}

#[test]
fn _3d_tensor_contraction() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [[[1, 2, 3], [4, 5, 6], [7, 8, 9]], [[10, 11, 12], [13, 14, 15], [16, 17, 18]], [[19, 20, 21], [22, 23, 24], [25, 26, 27]]];
                let b = [[[1, 2, 3], [4, 5, 6], [7, 8, 9]], [[10, 11, 12], [13, 14, 15], [16, 17, 18]], [[19, 20, 21], [22, 23, 24], [25, 26, 27]]];
                let c = [[[0, 0, 0], [0, 0, 0], [0, 0, 0]], [[0, 0, 0], [0, 0, 0], [0, 0, 0]], [[0, 0, 0], [0, 0, 0], [0, 0, 0]]];

                on self.gpus[0][i = 3][j = 3][k = 3] (c[i][j][k]) {
                    c[i][j][k] = 0;
                    for (let l = 0; l < 3; l = l + 1) {
                        c[i][j][k] = c[i][j][k] + a[i][j][l] * b[l][j][k];
                    }
                }

                return c[1][1][1];
            }
        "##},
        "main",
        606,
    );
}
