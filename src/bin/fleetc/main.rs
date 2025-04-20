use fleet::{self, tokenizer::Tokenizer};

fn main() {
    let src = include_str!("../../../test.fl");
    let mut tokenizer = Tokenizer::new(src.to_string());

    let tokens = tokenizer.tokenize().unwrap();
    println!("{:#?}", tokens);
}
