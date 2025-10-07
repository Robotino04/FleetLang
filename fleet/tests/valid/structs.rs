use indoc::indoc;

use crate::common::assert_compile_and_return_value;

#[test]
fn struct_as_parameter() {
    assert_compile_and_return_value(
        indoc! {r##"
            let modify_x = (a: struct {
                x: i32,
                y: i32,
            }) -> () {
                a.x = 5;
            }

            let main = () -> i32 {
                let s = idk {
                    x: 2,
                    y: 54,
                };

                modify_x(s);

                return s.x;
            }
        "##},
        "main",
        2,
    );
}
#[test]
fn struct_as_returnvalue() {
    assert_compile_and_return_value(
        indoc! {r##"
            let gen_struct = () -> struct {
                x: u8,
                y: f64,
            } {
                return idk {
                    x: 8,
                    y: 2.71828,
                };
            }

            let main = () -> i32 {
                let a = gen_struct();

                return a.x as i32;
            }
        "##},
        "main",
        8,
    );
}
#[test]
fn nested_struct() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let s = struct {
                    a: struct {
                        a1: f32,
                        a2: i16,
                    },
                    b: struct {
                        b1: u32,
                        b2: i32,
                    },
                } {
                    a: idk {
                        a1: 1.7,
                        a2: -5,
                    },
                    b: idk {
                        b1: 3,
                        b2: -11,
                    },
                };

                return s.a.a1 as i32 + s.a.a2 as i32 + s.b.b1 as i32 + s.b.b2;
            }
        "##},
        "main",
        -12,
    );
}
#[test]
fn array_in_struct() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let s = struct {
                    a: i32[5],
                } {
                    a: [7, -3, 0, 12, -8],
                };

                return s.a[3];
            }
        "##},
        "main",
        12,
    );
}
#[test]
fn array_in_struct_in_array() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let s: struct {
                    a: i32[5],
                }[2] = [idk {
                    a: [2, -7, 5, 9, -1],
                }, idk {
                    a: [-4, 3, -6, 11, 0],
                }];

                return s[1].a[3];
            }
        "##},
        "main",
        11,
    );
}
