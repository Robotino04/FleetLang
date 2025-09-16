use indoc::indoc;

use crate::common::{assert_compile_and_output_subprocess, assert_compile_and_return_value};

#[test]
fn _15d_array() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [[[[[[[[[[[[[[[15]]]]]]]]]]]]]]];
                return a[0][0][0][0][0][0][0][0][0][0][0][0][0][0][0];
            }
        "##},
        "main",
        15,
    );
}

#[test]
fn _3d_array() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [[[15]]];
                return a[0][0][0];
            }
        "##},
        "main",
        15,
    );
}

#[test]
fn _2d_array_in_expression_statement() {
    assert_compile_and_return_value(
        indoc! {r##"
            let foo = () -> () {
                [[1, 1], [2, 2], [3, 3], [4, 4], [5, 5]];
            }
            let main = () -> i32 {
                foo();
                return 1024;
            }
        "##},
        "main",
        1024,
    );
}

#[test]
fn _2d_array_variable() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [[77, 77], [5, 88], [99, 99]];
                return a[1][0];
            }
        "##},
        "main",
        5,
    );
}

#[test]
fn _2d_slice_assignment() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [[0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]];
                a[2] = [1, 2, 3];
                return a[2][1];
            }
        "##},
        "main",
        2,
    );
}

#[test]
fn array_access_in_function() {
    assert_compile_and_return_value(
        indoc! {r##"
            let foo = () -> i32 {
                // this will prob. segfault if the array does stupid stack manipulation
                return [1, 3, 8][1];
            }
            let main = () -> i32 {
                return foo();
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn array_access_nested() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 9, 0];
                let b = [[77, 77], [5, 88], [99, 99]];
                return b[a[0]][a[2]];
            }
        "##},
        "main",
        5,
    );
}

#[test]
fn expression_assignment_chaining() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [0, 0, 0];
                let b = [0, 0, 0];
                a = b = [1, 2, 3];
                return a[1] + b[2];
            }
        "##},
        "main",
        5,
    );
}

#[test]
fn complex_expression_assignment_chaining() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [0, 0, 0];
                let b = [0, 0, 0];
                let counter = 0;
                a = b = [counter = counter + 1, counter = counter + 1, counter = counter + 1];
                return counter;
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn array_in_expression_statement() {
    assert_compile_and_return_value(
        indoc! {r##"
            let foo = () -> () {
                [1, 2, 3, 4, 5];
            }
            let main = () -> i32 {
                foo();
                return 0;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn array_variable() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32[3] = [1, 2, 3];
                return a[1];
            }
        "##},
        "main",
        2,
    );
}

#[test]
fn index_assignment() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a = [0, 0, 0];
                a[2] = 91;
                return a[2];
            }
        "##},
        "main",
        91,
    );
}

#[test]
#[ignore = "Not sure how pointers will work and if they will be added at all"]
fn index_deref_array_in_function() {
    assert_compile_and_return_value(
        indoc! {r##"
            let index_deref_array_in_function = (array: *i32[5], param2: i32) -> i32{
                return (*array)[4];
            }
            let main = () -> i32 {
                numbers: i32[5] = [3, 118, 0, -1, 33];
                return index_deref_array_in_function($numbers, 1);
            }
        "##},
        "main",
        33,
    );
}

#[test]
fn index_literal() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return [1, 1 + 2, 9][1];
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn index_literal_2d() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return [[77, 77], [5, 88], [99, 99]][1][0];
            }
        "##},
        "main",
        5,
    );
}

#[test]
fn hello_world_array() {
    assert_compile_and_output_subprocess(
        indoc! {r##"
            let putchar = (char: i32) -> i32 @extern "putchar";
            let main = () -> i32 {
                let text = [72, 101, 108, 108, 111, 44, 32, 87, 111, 114, 108, 100, 33, 10];
                for (let i: u32 = 0; i < 14; i = i + 1) {
                    putchar(text[i]);
                }
                return 0;
            }
        "##},
        0,
        "Hello, World!\n",
        "",
    );
}

#[test]
fn empty_literal() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32[] = [];
                return 0;
            }
        "##},
        "main",
        0,
    );
}
