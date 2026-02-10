use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::assert_compile_error;

#[test]
fn bitwise_not_on_float() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> f32 {
                return ~12.0;
            }
        "##},
        SourceLocation {
            index: 35,
            line: 2,
            column: 12,
        },
    );
}
