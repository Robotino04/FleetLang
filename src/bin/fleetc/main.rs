use fleet::{self, tokenizer::Tokenizer, parser::Parser, pretty_print::PrettyPrint};

fn main() {
    let src = include_str!("../../../test.fl");
    let mut tokenizer = Tokenizer::new(src.to_string());

    let tokens = tokenizer.tokenize().unwrap();
    println!("{:#?}", tokens);

    println!("---------------------------------");

    let mut parser = Parser::new(tokens);
    let program = parser.parse_program();

    println!("{:#?}", program);

    println!("---------------------------------");

    println!("{}", program.unwrap().pretty_print());
}
