use fleet::tokenizer::SourceLocation;
use indoc::indoc;

use crate::common::assert_compile_error;

#[test]
fn missing_lvalue() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3];
                let b = [0, 0, 0];
                on self.gpus[0][i = 3] () {
                    b[i] = a[i];
                }
                return b[0] + b[1] + b[2];
            }
        "##},
        SourceLocation {
            index: 109,
            line: 5,
            column: 8,
        },
    );
}

#[test]
fn nonexistent_lvalue() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3];
                let b = [0, 0, 0];
                on self.gpus[0][i = 3] (c[i]) {
                    c[i] = a[i];
                }
                return b[0] + b[1] + b[2];
            }
        "##},
        SourceLocation {
            index: 97,
            line: 4,
            column: 28,
        },
    );
}

#[test]
fn unbound_index() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3];
                let b = [0, 0, 0];
                on self.gpus[0][i = 3] (b[i]) {
                    b[i + 1] = a[i];
                }
                return b[0] + b[1] + b[2];
            }
        "##},
        SourceLocation {
            index: 113,
            line: 5,
            column: 8,
        },
    );
}

#[test]
fn fixed_index() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3];
                let b = [0, 0, 0];
                on self.gpus[0][i = 3] (b[0]) {
                    b[i] = a[i];
                }
                return b[0] + b[1] + b[2];
            }
        "##},
        SourceLocation {
            index: 113,
            line: 5,
            column: 8,
        },
    );
}

#[test]
fn same_name_index() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                let a = [1, 2, 3];
                let b = [0, 0, 0];
                let c = [4, 5, 6];
                on self.gpus[0][i = 3] (b[i]) {
                    {
                        let i = 0;
                        b[i] = c[i] + a[i];
                    }
                }
                return b[0] + b[1] + b[2];
            }
        "##},
        SourceLocation {
            index: 173,
            line: 8,
            column: 12,
        },
    );
}

#[test]
fn no_iterators() {
    assert_compile_error(
        indoc! {r##"
            let main = () -> i32 {
                on self.gpus[0] () {}
                return 42;
            }
        "##},
        SourceLocation {
            index: 27,
            line: 2,
            column: 4,
        },
    );
}
