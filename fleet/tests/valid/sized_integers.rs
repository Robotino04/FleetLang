use indoc::indoc;

use crate::common::assert_compile_and_return_value;

#[test]
fn cast_i8_i32() {
    assert_compile_and_return_value(
        indoc! {r##"
            let foo = () -> i8 {
                return 10;
            }
            let main = () -> i32 {
                return foo() as i32;
            }
        "##},
        "main",
        10,
    );
}

#[test]
fn cast_i16_i32() {
    assert_compile_and_return_value(
        indoc! {r##"
            let foo = () -> i16 {
                return 10;
            }
            let main = () -> i32 {
                return foo() as i32;
            }
        "##},
        "main",
        10,
    );
}

#[test]
fn cast_i64_i32() {
    assert_compile_and_return_value(
        indoc! {r##"
            let foo = () -> i64 {
                return 10;
            }
            let main = () -> i32 {
                return foo() as i32;
            }
        "##},
        "main",
        10,
    );
}

#[test]
fn i32_i16_overwrite() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 10;
                let b: i16 = -2;
                return a;
            }
        "##},
        "main",
        10,
    );
}

#[test]
fn i32_i16_overwrite_assign() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 6;
                let b: i16 = 99;
                a = 10;
                b = -2;
                return a;
            }
        "##},
        "main",
        10,
    );
}

#[test]
fn overflow_cast_i16() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i64 {
                // 2^16 + 3
                return 65539 as i64 as i16 as i64;
            }
        "##},
        "main",
        3i64,
    );
}

#[test]
fn overflow_cast_i16_negative() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i64 {
                // -2^16
                return -65536 as i64 as i16 as i64;
            }
        "##},
        "main",
        0i64,
    );
}

#[test]
fn overflow_cast_i32() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i64 {
                // 2^32 + 1
                return 4294967297 as i64 as i32 as i64;
            }
        "##},
        "main",
        1i64,
    );
}

#[test]
fn overflow_cast_i32_negative() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i64 {
                // -2^32
                return -4294967296 as i64 as i32 as i64;
            }
        "##},
        "main",
        0i64,
    );
}

#[test]
fn overflow_cast_i8() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i64 {
                // 2^8 + 1
                return 257 as i64 as i8 as i64;
            }
        "##},
        "main",
        1i64,
    );
}

#[test]
fn overflow_cast_i8_negative() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i64 {
                // -2^8
                return -256 as i64 as i8 as i64;
            }
        "##},
        "main",
        0i64,
    );
}

#[test]
fn upcast_i16_i64() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i64 {
                return 5 as i16 as i64;
            }
        "##},
        "main",
        5i64,
    );
}

#[test]
fn upcast_i16_i64_negative() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i64 {
                return -5 as i16 as i64;
            }
        "##},
        // HACK: this doesn't initialize the FL runtime and is only done so we aren't
        // limited to main returning an i32. Tests should whenever possible call real main
        "fleet_main",
        -5i64,
    );
}

#[test]
fn upcast_i32_i64() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i64 {
                return 5 as i32 as i64;
            }
        "##},
        "main",
        5i64,
    );
}

#[test]
fn upcast_i32_i64_negative() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i64 {
                return -5 as i32 as i64;
            }
        "##},
        // HACK: this doesn't initialize the FL runtime and is only done so we aren't
        // limited to main returning an i32. Tests should whenever possible call real main
        "fleet_main",
        -5i64,
    );
}

#[test]
fn upcast_i8_i64() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i64 {
                return 5 as i8 as i64;
            }
        "##},
        "main",
        5i64,
    );
}

#[test]
fn upcast_i8_i64_negative() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i64 {
                return -5 as i8 as i64;
            }
        "##},
        // HACK: this doesn't initialize the FL runtime and is only done so we aren't
        // limited to main returning an i32. Tests should whenever possible call real main
        "fleet_main",
        -5i64,
    );
}

#[test]
fn unsized_int_expression() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                2;
                return 2;
            }
        "##},
        "main",
        2,
    );
}
