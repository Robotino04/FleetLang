use indoc::indoc;

use crate::common::{assert_compile_and_output_subprocess, assert_compile_and_return_value};

#[test]
fn main_returning_0() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 0;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn main_returning_5() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 5;
            }
        "##},
        "main",
        5,
    );
}

#[test]
fn two_functions() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return 5;
            }
            let main2 = () -> i32 {
                return 1;
            }
        "##},
        "main",
        5,
    );
}

#[test]
fn two_functions_reverse() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main2 = () -> i32 {
                return 5;
            }
            let main = () -> i32 {
                return 1;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn define_after() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return foo(3);
            }
            let foo = (a: i32) -> i32 {
                return a + 1;
            }
        "##},
        "main",
        4,
    );
}

#[test]
fn define_before() {
    assert_compile_and_return_value(
        indoc! {r##"
            let foo = (a: i32) -> i32 {
                return a + 1;
            }
            let main = () -> i32 {
                return foo(4);
            }
        "##},
        "main",
        5,
    );
}

#[test]
fn expression_args() {
    assert_compile_and_return_value(
        indoc! {r##"
            let add = (a: i32, b: i32) -> i32 {
                return a + b;
            }
            let main = () -> i32 {
                let sum: i32 = add(1 + 2, 4);
                return sum + sum;
            }
        "##},
        "main",
        14,
    );
}

#[test]
fn fib() {
    assert_compile_and_return_value(
        indoc! {r##"
            let fib = (n: i32) -> i32 {
                if n == 0 || n == 1 {
                    return n;
                }
                else {
                    return fib(n - 1) + fib(n - 2);
                }
            }
            let main = () -> i32 {
                let n: i32 = 5;
                return fib(n);
            }
        "##},
        "main",
        5,
    );
}

#[test]
fn fun_in_expr() {
    assert_compile_and_return_value(
        indoc! {r##"
            let sum = (a: i32, b: i32) -> i32 {
                return a + b;
            }
            let main = () -> i32 {
                let a: i32 = sum(1, 2) - sum(1, 2) / 2 * 2;
                let b: i32 = 2 * sum(3, 4) + sum(1, 2);
                return b - a;
            }
        "##},
        "main",
        16,
    );
}

#[test]
fn hello_world() {
    assert_compile_and_output_subprocess(
        indoc! {r##"
            let putchar = (char: i32) -> i32 @extern "putchar";
            let main = () -> i32 {
                putchar(72); // 'H'
                putchar(101); // 'e'
                putchar(108); // 'l'
                putchar(108); // 'l'
                putchar(111); // 'o'
                putchar(44); // ','
                putchar(32); // ' '
                putchar(87); // 'W'
                putchar(111); // 'o'
                putchar(114); // 'r'
                putchar(108); // 'l'
                putchar(100); // 'd'
                putchar(33); // '!'
                putchar(10); // '\n'
                return 0;
            }
        "##},
        0,
        "Hello, World!\n",
        "",
    );
}

#[test]
fn multi_arg() {
    assert_compile_and_return_value(
        indoc! {r##"
            let sub_3 = (x: i32, y: i32, z: i32) -> i32 {
                return x - y - z;
            }
            let main = () -> i32 {
                return sub_3(10, 4, 2);
            }
        "##},
        "main",
        4,
    );
}

#[test]
fn mutual_recursion() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                return foo(5);
            }
            let foo = (a: i32) -> i32 {
                if a <= 0 {
                    return a;
                }
                return a + bar(a - 1);
            }
            let bar = (b: i32) -> i32 {
                if b <= 0 {
                    return b;
                }
                return b + bar(b / 2);
            }
        "##},
        "main",
        12,
    );
}

#[test]
fn no_arg() {
    assert_compile_and_return_value(
        indoc! {r##"
            let three = () -> i32 {
                return 3;
            }
            let main = () -> i32 {
                return three();
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn precedence() {
    assert_compile_and_return_value(
        indoc! {r##"
            let three = () -> i32 {
                return 3;
            }
            let main = () -> bool {
                return !three();
            }
        "##},
        "main",
        false,
    );
}

#[test]
fn single_arg() {
    assert_compile_and_return_value(
        indoc! {r##"
            let twice = (x: i32) -> i32 {
                return 2 * x;
            }
            let main = () -> i32 {
                return twice(3);
            }
        "##},
        "main",
        6,
    );
}

#[test]
fn stack_pointer_alignment_direction() {
    assert_compile_and_return_value(
        indoc! {r##"
            let fill_stack = () -> () {
                let _0: i8 = -22;
                _0 = 15;
                let _1: i8 = -22;
                _1 = 15;
                let _2: i8 = -22;
                _2 = 15;
                let _3: i8 = -22;
                _3 = 15;
                let _4: i8 = -22;
                _4 = 15;
                let _5: i8 = -22;
                _5 = 15;
                let _6: i8 = -22;
                _6 = 15;
                let _7: i8 = -22;
                _7 = 15;
                let _8: i8 = -22;
                _8 = 15;
                let _9: i8 = -22;
                _9 = 15;
                let _10: i8 = -22;
                _10 = 15;
                let _11: i8 = -22;
                _11 = 15;
                let _12: i8 = -22;
                _12 = 15;
                let _13: i8 = -22;
                _13 = 15;
                let _14: i8 = -22;
                _14 = 15;
                let _15: i8 = -22;
                _15 = 15;
                let _16: i8 = -22;
                _16 = 15;
                let _17: i8 = -22;
                _17 = 15;
                let _18: i8 = -22;
                _18 = 15;
                let _19: i8 = -22;
                _19 = 15;
                let _20: i8 = -22;
                _20 = 15;
                let _21: i8 = -22;
                _21 = 15;
                let _22: i8 = -22;
                _22 = 15;
                let _23: i8 = -22;
                _23 = 15;
                let _24: i8 = -22;
                _24 = 15;
                let _25: i8 = -22;
                _25 = 15;
                let _26: i8 = -22;
                _26 = 15;
                let _27: i8 = -22;
                _27 = 15;
                let _28: i8 = -22;
                _28 = 15;
                let _29: i8 = -22;
                _29 = 15;
                let _30: i8 = -22;
                _30 = 15;
                let _31: i8 = -22;
                _31 = 15;
                let _32: i8 = -22;
                _32 = 15;
                let _33: i8 = -22;
                _33 = 15;
                let _34: i8 = -22;
                _34 = 15;
                let _35: i8 = -22;
                _35 = 15;
                let _36: i8 = -22;
                _36 = 15;
                let _37: i8 = -22;
                _37 = 15;
                let _38: i8 = -22;
                _38 = 15;
                let _39: i8 = -22;
                _39 = 15;
                let _40: i8 = -22;
                _40 = 15;
                let _41: i8 = -22;
                _41 = 15;
                let _42: i8 = -22;
                _42 = 15;
                let _43: i8 = -22;
                _43 = 15;
                let _44: i8 = -22;
                _44 = 15;
                let _45: i8 = -22;
                _45 = 15;
                let _46: i8 = -22;
                _46 = 15;
                let _47: i8 = -22;
                _47 = 15;
                let _48: i8 = -22;
                _48 = 15;
                let _49: i8 = -22;
                _49 = 15;
                let _50: i8 = -22;
                _50 = 15;
                let _51: i8 = -22;
                _51 = 15;
                let _52: i8 = -22;
                _52 = 15;
                let _53: i8 = -22;
                _53 = 15;
                let _54: i8 = -22;
                _54 = 15;
                let _55: i8 = -22;
                _55 = 15;
                let _56: i8 = -22;
                _56 = 15;
                let _57: i8 = -22;
                _57 = 15;
                let _58: i8 = -22;
                _58 = 15;
                let _59: i8 = -22;
                _59 = 15;
                let _60: i8 = -22;
                _60 = 15;
                let _61: i8 = -22;
                _61 = 15;
                let _62: i8 = -22;
                _62 = 15;
                let _63: i8 = -22;
                _63 = 15;
                let _64: i8 = -22;
                _64 = 15;
                let _65: i8 = -22;
                _65 = 15;
                let _66: i8 = -22;
                _66 = 15;
                let _67: i8 = -22;
                _67 = 15;
                let _68: i8 = -22;
                _68 = 15;
                let _69: i8 = -22;
                _69 = 15;
                let _70: i8 = -22;
                _70 = 15;
                let _71: i8 = -22;
                _71 = 15;
                let _72: i8 = -22;
                _72 = 15;
                let _73: i8 = -22;
                _73 = 15;
                let _74: i8 = -22;
                _74 = 15;
                let _75: i8 = -22;
                _75 = 15;
                let _76: i8 = -22;
                _76 = 15;
                let _77: i8 = -22;
                _77 = 15;
                let _78: i8 = -22;
                _78 = 15;
                let _79: i8 = -22;
                _79 = 15;
                let _80: i8 = -22;
                _80 = 15;
                let _81: i8 = -22;
                _81 = 15;
                let _82: i8 = -22;
                _82 = 15;
                let _83: i8 = -22;
                _83 = 15;
                let _84: i8 = -22;
                _84 = 15;
                let _85: i8 = -22;
                _85 = 15;
                let _86: i8 = -22;
                _86 = 15;
                let _87: i8 = -22;
                _87 = 15;
                let _88: i8 = -22;
                _88 = 15;
                let _89: i8 = -22;
                _89 = 15;
                let _90: i8 = -22;
                _90 = 15;
                let _91: i8 = -22;
                _91 = 15;
                let _92: i8 = -22;
                _92 = 15;
                let _93: i8 = -22;
                _93 = 15;
                let _94: i8 = -22;
                _94 = 15;
                let _95: i8 = -22;
                _95 = 15;
                let _96: i8 = -22;
                _96 = 15;
                let _97: i8 = -22;
                _97 = 15;
                let _98: i8 = -22;
                _98 = 15;
                let _99: i8 = -22;
                _99 = 15;
            }
            // simplified version of greetings example (from R#)
            let main = () -> i32 {
                let name_buffer_length: i32 = 50;
                let name: i64 = 0;
                let name_length: i32 = 0;
                fill_stack();
                return name_buffer_length - name as i32 - name_length;
            }
        "##},
        "main",
        50,
    );
}

#[test]
fn stack_variable_after_return() {
    assert_compile_and_return_value(
        indoc! {r##"
            let recurse = (depth: i32) -> () {
                if depth == 0 {
                    return;
                }
                let new_depth: i32 = depth - 1;
                recurse(new_depth);
            }
            let main = () -> i32 {
                recurse(500);
                return 0;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn variable_as_arg() {
    assert_compile_and_return_value(
        indoc! {r##"
            let foo = (x: i32) -> i32 {
                return x + 1;
            }
            let main = () -> i32 {
                let a: i32 = 1;
                return foo(a);
            }
        "##},
        "main",
        2,
    );
}

#[test]
fn shadow_parameter() {
    assert_compile_and_return_value(
        indoc! {r##"
            let foo = (x: i32) -> i32 {
                let x: i32 = 3;
                return x;
            }
            let main = () -> i32 {
                return foo(4);
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn unit_no_return() {
    assert_compile_and_return_value(
        indoc! {r##"
            let foo = () -> () {
            }
            let main = () -> i32 {
                foo();
                return 3;
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn unit_return() {
    assert_compile_and_return_value(
        indoc! {r##"
            let foo = () -> () {
                return;
            }
            let main = () -> i32 {
                foo();
                return 3;
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn extern_function() {
    assert_compile_and_output_subprocess(
        indoc! {r##"
            let putchar = (char: i32) -> i32 @extern "putchar";
            let main = () -> i32 {
                putchar(10);
                return 0;
            }
        "##},
        0,
        "\n",
        "",
    );
}

#[test]
fn extern_function_renamed() {
    assert_compile_and_output_subprocess(
        indoc! {r##"
            let not_putchar = (char: i32) -> i32 @extern "putchar";
            let main = () -> i32 {
                not_putchar(10);
                return 0;
            }
        "##},
        0,
        "\n",
        "",
    );
}
