use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::assert_compile_error;

#[test]
fn mixed_missing_return_type() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> {
                if true {
                    return 2 as i32;
                }
                else {
                    return 9 as i16;
                }
            }
        "##},
        SourceLocation {
            index: 83,
            line: 6,
            column: 8,
        },
    );
}

#[test]
fn missing_parameter_type() {
    // may get lifted in the future if lambdas are implemented
    assert_compile_error(
        indoc! {r##"
            let foo = (a) -> {
                return a;
            }
            let main = () -> i32 {
                return foo(2);
            }
        "##},
        SourceLocation {
            index: 11,
            line: 1,
            column: 11,
        },
    );
}
