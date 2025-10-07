use crate::common::assert_compile_and_output_subprocess;

#[test]
fn hello_world() {
    assert_compile_and_output_subprocess(include_str!("hello_world.fl"), 0, "Hello, World!\n", "");
}
