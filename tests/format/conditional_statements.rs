use indoc::indoc;

use crate::common::assert_formatting_and_same_behaviour;

#[test]
fn expand_if() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                if 0{return 0;}
                return 1;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                if 0 {
                    return 0;
                }
                return 1;
            }"##
        },
        "main",
    );
}

#[test]
fn expand_if_else() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                if 0{return 0;}else{return 2;}
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                if 0 {
                    return 0;
                }
                else {
                    return 2;
                }
            }"##
        },
        "main",
    );
}

#[test]
fn expand_if_elif_else() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                if 0{return 0;}elif 2{return 3;}else{return 2;}
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                if 0 {
                    return 0;
                }
                elif 2 {
                    return 3;
                }
                else {
                    return 2;
                }
            }"##
        },
        "main",
    );
}

#[test]
fn expand_if_elif_elif_else() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                if 0{return 0;}elif 2{return 3;}elif 8{return 4;}else{return 2;}
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                if 0 {
                    return 0;
                }
                elif 2 {
                    return 3;
                }
                elif 8 {
                    return 4;
                }
                else {
                    return 2;
                }
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_if() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                if   0   {


                return 0;

                }
                return 1;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                if 0 {
                    return 0;
                }
                return 1;
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_if_else() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                if 

                0  
                {   return 0;
                }

                else{
                return 2;  }
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                if 0 {
                    return 0;
                }
                else {
                    return 2;
                }
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_if_elif_else() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                if 0

                {  return 0; 
                }  elif   2 
                {
                return 3;
                }
                else     {return 2;
                }
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                if 0 {
                    return 0;
                }
                elif 2 {
                    return 3;
                }
                else {
                    return 2;
                }
            }"##
        },
        "main",
    );
}

#[test]
fn collapse_if_elif_elif_else() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                if 0

                {  return 0; 
                }  elif   2 
                {
                return 3;
                }  elif   8 
                {
                return 4;
                }

                else     {return 2;
                }
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                if 0 {
                    return 0;
                }
                elif 2 {
                    return 3;
                }
                elif 8 {
                    return 4;
                }
                else {
                    return 2;
                }
            }"##
        },
        "main",
    );
}

#[test]
fn remove_parens_if() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                if (((1+1))*2) {
                    return 0;
                }
                return 2;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                if (1 + 1) * 2 {
                    return 0;
                }
                return 2;
            }"##
        },
        "main",
    );
}

#[test]
fn remove_parens_elif() {
    assert_formatting_and_same_behaviour::<i32>(
        indoc! {r##"
            let main = () -> i32 {
                if 0 {
                    return 3;
                }
                elif (((1+1))*2) {
                    return 0;
                }
                return 2;
            }"##
        },
        indoc! {r##"
            let main = () -> i32 {
                if 0 {
                    return 3;
                }
                elif (1 + 1) * 2 {
                    return 0;
                }
                return 2;
            }"##
        },
        "main",
    );
}
