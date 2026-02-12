use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::assert_compile_and_warning;

#[test]
fn variable_not_snake_case() {
    assert_compile_and_warning(
        indoc! {r##"
            let main = () -> i32 {
                let OneVeryCoolVariable = 0;
                return OneVeryCoolVariable;
            }
        "##},
        SourceLocation {
            index: 31,
            line: 2,
            column: 8,
        },
    );
}

#[test]
fn function_not_snake_case() {
    assert_compile_and_warning(
        indoc! {r##"
            let Foo = () -> i32 {
                return 2;
            }

            let main = () -> i32 {
                return Foo();
            }
        "##},
        SourceLocation {
            index: 4,
            line: 1,
            column: 4,
        },
    );
}

#[test]
fn type_not_camel_case() {
    assert_compile_and_warning(
        indoc! {r##"
            let foo = struct {
                bar: i32
            };

            let main = () -> i32 {
                let my_foo = foo {
                    bar: 5
                };

                return my_foo.bar;
            }
        "##},
        SourceLocation {
            index: 4,
            line: 1,
            column: 4,
        },
    );
}

#[test]
fn struct_member_not_snake_case() {
    assert_compile_and_warning(
        indoc! {r##"
            let Foo = struct {
                Bar: i32
            };

            let main = () -> i32 {
                let my_foo = Foo {
                    Bar: 5
                };

                return my_foo.Bar;
            }
        "##},
        SourceLocation {
            index: 23,
            line: 2,
            column: 4,
        },
    );
}
