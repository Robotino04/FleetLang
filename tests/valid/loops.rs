use indoc::indoc;

use crate::common::assert_compile_and_return_value;

#[test]
fn break_for() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let sum: i32 = 0;
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    sum = sum + i;
                    if sum > 10 {
                        break;
                    }
                }
                return sum;
            }
        "##},
        "main",
        15,
    );
}

#[test]
fn for_counter_allocation() {
    assert_compile_and_return_value(
        indoc! {r##"
            let function = (param: i32) -> i32 {
                let some_var: i32 = 10;
                // this hung in R#, because i was not being allocated and always read some_var, which never reached 0.
                for (let i: i32 = 5; i != 0; i = i - 1) {
                }
                return some_var;
            }
            let main = () -> i32 {
                let tmp: i32 = function(123);
                return tmp;
            }
        "##},
        "main",
        10,
    );
}

#[test]
fn for_() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                for (let i: i32 = 0; i < 3; i = i + 1) {
                    a = a + 1;
                }
                return a;
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn for_stack_cleanup() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    let var: i32 = 2;
                    break;
                }
                return 0;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn for_nested_scope() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let i: i32 = 0;
                let j: i32 = 0;
                for (let i: i32 = 100; i > 0; i = i - 1) {
                    let i: i32 = 0;
                    let j: i32 = j * 2 + i;
                }
                let k: i32 = 3;
                return j + k;
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn for_variable_shadowing() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let i: i32 = 0;
                let j: i32 = 0;
                for (i = 0; i < 10; i = i + 1) {
                    let k: i32 = i;
                    for (let i: i32 = k; i < 10; i = i + 1) {
                        j = j + 1;
                    }
                }
                return j + i;
            }
        "##},
        "main",
        65,
    );
}

#[test]
fn nested_break() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let ans: i32 = 0;
                for (let i: i32 = 0; i < 5; i = i + 1) {
                    for (let j: i32 = 0; j < 10; j = j + 1) {
                        if i % 2 == 0 {
                            break;
                        }
                        else {
                            ans = ans + i;
                        }
                    }
                }
                return ans;
            }
        "##},
        "main",
        40,
    );
}

#[test]
fn nested_while() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 1;
                while a / 3 < 20 {
                    let b: i32 = 1;
                    while b < 10 {
                        b = b * 2;
                    }
                    a = a + b;
                }
                return a;
            }
        "##},
        "main",
        65,
    );
}

#[test]
fn return_in_while() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                while 1 {
                    return 2;
                }
            }
        "##},
        "main",
        2,
    );
}

#[test]
fn skip() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let sum: i32 = 0;
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    if sum % 2 == 1 {
                        skip;
                    }
                    sum = sum + i;
                }
                return sum;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn skip_empty_post() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let sum: i32 = 0;
                for (let i: i32 = 0; i < 10;) {
                    i = i + 1;
                    if sum % 2 == 1 {
                        skip;
                    }
                    sum = sum + i;
                }
                return sum;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn while_multi_statement() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                let b: i32 = 0;
                while a < 5 {
                    a = a + 2;
                    b = b * a;
                }
                return a;
            }
        "##},
        "main",
        6,
    );
}
