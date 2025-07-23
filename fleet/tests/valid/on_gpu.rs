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
