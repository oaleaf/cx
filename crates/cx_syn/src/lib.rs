use lalrpop_util::lalrpop_mod;

use ast::SourceFileRoot;

lalrpop_mod!(cx_grammar);

pub mod ast;
mod lexer;

pub fn parse(input: &str) -> SourceFileRoot {
    let input = input.trim_start_matches(|c: char| c.is_whitespace() || c == '\n');
    dbg!(input);
    cx_grammar::SourceFileRootParser::new()
        .parse(input, lexer::Lexer::new(input))
        .unwrap()
}
