use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::assert_compile_error;

#[test]
fn redefine_type_alias() {
    assert_compile_error(
        indoc! {r##"
            let Vec3 = struct {
                x: f32,
                y: f32,
                z: f32,
            };
            let Vec3 = struct {
                x: f32,
                y: f32,
                z: f32,
            };

            let main = () -> i32 {
                return 0;
            }
        "##},
        SourceLocation {
            index: 63,
            line: 6,
            column: 4,
        },
    );
}
#[test]
fn duplicated_members() {
    assert_compile_error(
        indoc! {r##"
            let Vec3 = struct {
                x: f32,
                y: f32,
                y: f32,
            };

            let main = () -> i32 {
                return 0;
            }
        "##},
        SourceLocation {
            index: 48,
            line: 4,
            column: 4,
        },
    );
}
