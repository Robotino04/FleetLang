use fleet::{ast::AstNode, parser::Parser, pretty_print::pretty_print, tokenizer::Tokenizer};

fn main() {
    let src = include_str!("../../../test.fl");
    let mut tokenizer = Tokenizer::new(src.to_string());

    let tokens = tokenizer.tokenize().unwrap();
    println!("{:#?}", tokens);

    println!("---------------------------------");

    let mut parser = Parser::new(tokens);
    let program = parser.parse_program().unwrap();

    println!("{:#?}", program);

    println!("---------------------------------");

    println!("{}", pretty_print(AstNode::Program(program)));
}
