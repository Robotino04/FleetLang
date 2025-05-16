use indoc::indoc;

use crate::common::assert_compile_and_return_value;

#[test]
fn assign() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 2;
                return a;
            }
        "##},
        "main",
        2,
    );
}

#[test]
fn assign_val() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 2;
                let b: i32 = a = 0;
                return b;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn big_stack() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let _0: i32 = -22;
                _0 = 15;
                let _1: i32 = -22;
                _1 = 15;
                let _2: i32 = -22;
                _2 = 15;
                let _3: i32 = -22;
                _3 = 15;
                let _4: i32 = -22;
                _4 = 15;
                let _5: i32 = -22;
                _5 = 15;
                let _6: i32 = -22;
                _6 = 15;
                let _7: i32 = -22;
                _7 = 15;
                let _8: i32 = -22;
                _8 = 15;
                let _9: i32 = -22;
                _9 = 15;
                let _10: i32 = -22;
                _10 = 15;
                let _11: i32 = -22;
                _11 = 15;
                let _12: i32 = -22;
                _12 = 15;
                let _13: i32 = -22;
                _13 = 15;
                let _14: i32 = -22;
                _14 = 15;
                let _15: i32 = -22;
                _15 = 15;
                let _16: i32 = -22;
                _16 = 15;
                let _17: i32 = -22;
                _17 = 15;
                let _18: i32 = -22;
                _18 = 15;
                let _19: i32 = -22;
                _19 = 15;
                let _20: i32 = -22;
                _20 = 15;
                let _21: i32 = -22;
                _21 = 15;
                let _22: i32 = -22;
                _22 = 15;
                let _23: i32 = -22;
                _23 = 15;
                let _24: i32 = -22;
                _24 = 15;
                let _25: i32 = -22;
                _25 = 15;
                let _26: i32 = -22;
                _26 = 15;
                let _27: i32 = -22;
                _27 = 15;
                let _28: i32 = -22;
                _28 = 15;
                let _29: i32 = -22;
                _29 = 15;
                let _30: i32 = -22;
                _30 = 15;
                let _31: i32 = -22;
                _31 = 15;
                let _32: i32 = -22;
                _32 = 15;
                let _33: i32 = -22;
                _33 = 15;
                let _34: i32 = -22;
                _34 = 15;
                let _35: i32 = -22;
                _35 = 15;
                let _36: i32 = -22;
                _36 = 15;
                let _37: i32 = -22;
                _37 = 15;
                let _38: i32 = -22;
                _38 = 15;
                let _39: i32 = -22;
                _39 = 15;
                let _40: i32 = -22;
                _40 = 15;
                let _41: i32 = -22;
                _41 = 15;
                let _42: i32 = -22;
                _42 = 15;
                let _43: i32 = -22;
                _43 = 15;
                let _44: i32 = -22;
                _44 = 15;
                let _45: i32 = -22;
                _45 = 15;
                let _46: i32 = -22;
                _46 = 15;
                let _47: i32 = -22;
                _47 = 15;
                let _48: i32 = -22;
                _48 = 15;
                let _49: i32 = -22;
                _49 = 15;
                let _50: i32 = -22;
                _50 = 15;
                let _51: i32 = -22;
                _51 = 15;
                let _52: i32 = -22;
                _52 = 15;
                let _53: i32 = -22;
                _53 = 15;
                let _54: i32 = -22;
                _54 = 15;
                let _55: i32 = -22;
                _55 = 15;
                let _56: i32 = -22;
                _56 = 15;
                let _57: i32 = -22;
                _57 = 15;
                let _58: i32 = -22;
                _58 = 15;
                let _59: i32 = -22;
                _59 = 15;
                let _60: i32 = -22;
                _60 = 15;
                let _61: i32 = -22;
                _61 = 15;
                let _62: i32 = -22;
                _62 = 15;
                let _63: i32 = -22;
                _63 = 15;
                let _64: i32 = -22;
                _64 = 15;
                let _65: i32 = -22;
                _65 = 15;
                let _66: i32 = -22;
                _66 = 15;
                let _67: i32 = -22;
                _67 = 15;
                let _68: i32 = -22;
                _68 = 15;
                let _69: i32 = -22;
                _69 = 15;
                let _70: i32 = -22;
                _70 = 15;
                let _71: i32 = -22;
                _71 = 15;
                let _72: i32 = -22;
                _72 = 15;
                let _73: i32 = -22;
                _73 = 15;
                let _74: i32 = -22;
                _74 = 15;
                let _75: i32 = -22;
                _75 = 15;
                let _76: i32 = -22;
                _76 = 15;
                let _77: i32 = -22;
                _77 = 15;
                let _78: i32 = -22;
                _78 = 15;
                let _79: i32 = -22;
                _79 = 15;
                let _80: i32 = -22;
                _80 = 15;
                let _81: i32 = -22;
                _81 = 15;
                let _82: i32 = -22;
                _82 = 15;
                let _83: i32 = -22;
                _83 = 15;
                let _84: i32 = -22;
                _84 = 15;
                let _85: i32 = -22;
                _85 = 15;
                let _86: i32 = -22;
                _86 = 15;
                let _87: i32 = -22;
                _87 = 15;
                let _88: i32 = -22;
                _88 = 15;
                let _89: i32 = -22;
                _89 = 15;
                let _90: i32 = -22;
                _90 = 15;
                let _91: i32 = -22;
                _91 = 15;
                let _92: i32 = -22;
                _92 = 15;
                let _93: i32 = -22;
                _93 = 15;
                let _94: i32 = -22;
                _94 = 15;
                let _95: i32 = -22;
                _95 = 15;
                let _96: i32 = -22;
                _96 = 15;
                let _97: i32 = -22;
                _97 = 15;
                let _98: i32 = -22;
                _98 = 15;
                let _99: i32 = -22;
                _99 = 15;
                return _0 + _99;
            }
        "##},
        "main",
        30,
    );
}

#[test]
fn big_stack_used() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 1;
                let b: i32 = 2;
                let c: i32 = 3;
                let d: i32 = 4;
                let e: i32 = 5;
                let f: i32 = 6;
                let g: i32 = 7;
                let h: i32 = 8;
                let i: i32 = 9;
                let j: i32 = 10;
                let k: i32 = 11;
                let l: i32 = 12;
                let m: i32 = 13;
                let n: i32 = 14;
                let o: i32 = 15;
                let p: i32 = 16;
                let q: i32 = 17;
                let r: i32 = 18;
                let s: i32 = 19;
                let t: i32 = 20;
                let u: i32 = 21;
                let v: i32 = 22;
                let w: i32 = 23;
                let x: i32 = 24;
                let y: i32 = 25;
                let z: i32 = 26;
                // mod 120 so it fits in linux' exit code limit + some extra to be safe
                return (a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r + s + t + u + v + w + x + y + z) % 120;
            }
        "##},
        "main",
        111,
    );
}

#[test]
fn exp_return_value() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                let b: i32 = 0;
                a = b = 4;
                return a - b;
            }
        "##},
        "main",
        0,
    );
}

#[test]
fn multiple_vars() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 1;
                let b: i32 = 2;
                return a + b;
            }
        "##},
        "main",
        3,
    );
}

#[test]
fn refer() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 2;
                return a;
            }
        "##},
        "main",
        2,
    );
}

#[test]
fn unused_variable() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 1;
                let _unused: i32 = 0;
                return a;
            }
        "##},
        "main",
        1,
    );
}

#[test]
fn assign_precedence1() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                (a = 2) + 3;
                return a;
            }
        "##},
        "main",
        2,
    );
}

#[test]
fn assign_precedence2() {
    assert_compile_and_return_value(
        indoc! {r##"
            let main = () -> i32 {
                let a: i32 = 0;
                a = 2 + 3;
                return a;
            }
        "##},
        "main",
        5,
    );
}
