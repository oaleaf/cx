use logos::Logos;

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Tk {
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,
    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("::")]
    Colon2,
    #[token("#")]
    Hash,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Asterisk,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("&&")]
    Amp2,
    #[token("||")]
    Pipe2,
    #[token("!")]
    Bang,
    #[token("@")]
    At,
    #[token("=")]
    Eq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Lte,
    #[token(">=")]
    Gte,
    #[token("==")]
    Eq2,
    #[token("!=")]
    BangEq,
    #[token("fn")]
    FnKw,
    #[token("struct")]
    StructKw,
    #[token("usn")]
    UsnKw,
    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Name,
    #[regex("0b[01]+(i8|u8|i16|u16|i32|u32|i64|u64|usize|isize)?")]
    BinNum,
    #[regex("0[0-7_]*(i8|u8|i16|u16|i32|u32|i64|u64|usize|isize)?")]
    OctNum,
    #[regex("[1-9][0-9_]*(i8|u8|i16|u16|i32|u32|i64|u64|usize|isize)?")]
    DecNum,
    #[regex("0x[0-9a-fA-F_]+(i8|u8|i16|u16|i32|u32|i64|u64|usize|isize)?")]
    HexNum,
    #[token("\n")]
    Newline,
    #[regex(r"[ \t]+")]
    Whitespace,

    NewlineContinuation,

    #[error]
    Error,
}

bitflags::bitflags! {
    pub struct WsClass: u8 {
        const WS = 0b0000_0001;
        const NEWLINE = 0b0000_0010;
        const CONTINUATION = 0b0000_0100;
    }
}

pub struct Lexer<'input> {
    input: &'input str,
    inner: logos::Lexer<'input, Tk>,

    pb: Vec<(usize, Token<'input>, usize)>,

    paren_stack: ParenStack,
    ws_state: LexerWhitespaceState,
}

pub struct ParenStack {
    stack: Vec<ParenKind>,
}

impl ParenStack {
    pub fn new() -> Self {
        ParenStack { stack: Vec::new() }
    }

    pub fn push(&mut self, kind: ParenKind) {
        self.stack.push(kind);
    }

    pub fn pop(&mut self) -> Option<ParenKind> {
        self.stack.pop()
    }

    pub fn peek(&self) -> Option<ParenKind> {
        self.stack.last().copied()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParenKind {
    Parentheses,
    Brackets,
    Braces,
}

impl From<Tk> for ParenKind {
    fn from(tk: Tk) -> Self {
        match tk {
            Tk::OpenParen | Tk::CloseParen => Self::Parentheses,
            Tk::OpenBracket | Tk::CloseBracket => Self::Brackets,
            Tk::OpenBrace | Tk::CloseBrace => Self::Braces,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token<'input> {
    pub tk: Tk,
    pub input: &'input str,
}

impl<'input> Lexer<'input> {
    pub fn new(source: &'input str) -> Self {
        Self {
            input: source,
            inner: Tk::lexer(source),
            paren_stack: ParenStack::new(),
            ws_state: LexerWhitespaceState {
                prev_indent: 0,
                current_indent: 0,
                prev_brace: false,
                last_newline: true,
                continuation_indent: 0,
                no_continuation_indent: 0,
            },
            pb: Vec::new(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(usize, Token<'input>, usize), ()>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(pb) = self.pb.pop() {
            return Some(Ok(pb));
        }

        let tk = self.inner.next()?;
        let input = self.inner.slice();
        let span = self.inner.span();

        match tk {
            Tk::OpenParen | Tk::OpenBracket => {
                self.ws_state.last_newline = false;
                self.ws_state.prev_brace = false;
                self.paren_stack.push(ParenKind::from(tk));
            }
            Tk::CloseParen | Tk::CloseBracket => {
                self.ws_state.last_newline = false;
                self.ws_state.prev_brace = false;
                self.paren_stack.pop();
            }
            Tk::OpenBrace => {
                self.ws_state.last_newline = false;
                self.ws_state.prev_brace = true;
                self.paren_stack.push(ParenKind::Braces);
            }
            Tk::CloseBrace => {
                self.ws_state.last_newline = false;
                self.ws_state.prev_brace = false;
                self.paren_stack.pop();
            }
            Tk::Newline => {
                let peek = self.paren_stack.peek();
                if matches!(peek, Some(pk) if pk != ParenKind::Braces) {
                    // in () / [], parser doesn't expect new lines
                    return self.next();
                }

                self.ws_state.last_newline = true;
                self.ws_state.prev_indent = self.ws_state.current_indent;
                self.ws_state.current_indent = 0;

                let prev_brace = self.ws_state.prev_brace;

                match self.next() {
                    Some(Ok(
                        ws @ (
                            _,
                            Token {
                                tk: Tk::Whitespace,
                                input: ws_input,
                            },
                            r,
                        ),
                    )) => {
                        self.ws_state.current_indent = ws_input.len();

                        if prev_brace {
                            self.ws_state.no_continuation_indent = self.ws_state.current_indent;

                            // do not push the whitespace back

                            // no continuation right after braces
                            return Some(Ok((span.start, Token { tk, input }, span.end)));
                        }

                        if self.ws_state.current_indent <= self.ws_state.no_continuation_indent {
                            // do not push the whitespace back

                            // no continuation
                            return Some(Ok((span.start, Token { tk, input }, span.end)));
                        }

                        if self.ws_state.current_indent >= self.ws_state.prev_indent + 4
                            || self.ws_state.current_indent >= self.ws_state.continuation_indent
                        {
                            self.ws_state.continuation_indent = self.ws_state.current_indent;
                            return Some(Ok((
                                span.start,
                                Token {
                                    tk: Tk::NewlineContinuation,
                                    input: &self.input[span.start..r],
                                },
                                r,
                            )));
                        } else {
                            self.pb.push(ws);
                        }
                    }
                    Some(Ok(other)) => {
                        self.pb.push(other);
                    }
                    _ => {}
                }

                self.ws_state.last_newline = true;
            }
            Tk::Whitespace => {
                if !self.ws_state.last_newline {
                    return self.next(); // ignore whitespace if not after newline
                }
            }
            _ => {
                self.ws_state.last_newline = false;
                self.ws_state.prev_brace = false;
            }
        }

        Some(Ok((span.start, Token { tk, input }, span.end)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct LexerWhitespaceState {
    // the indentation of the previous line
    prev_indent: usize,
    // the indentation of the current line
    current_indent: usize,
    // whether the last non-whitespace token was a curly brace,
    // in which case a `NewlineContinuation` should not be emitted
    prev_brace: bool,
    // whether or not the last token was a newline
    last_newline: bool,
    // the indentation of the first continuation newline
    continuation_indent: usize,
    // the indentation of the first non-continuation newline
    no_continuation_indent: usize,
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, Tk, Token};

    macro_rules! assert_token_stream {
        ($source:expr, [$($v:tt),* $(,)?]) => {
            let mut lexer = Lexer::new($source);
            assert_token_stream!(@asserts $source lexer [$($v)*]);
        };

        (@asserts $source:literal $lexer:ident [$($v:tt)*]) => {
            let mut tok_pos = 0;

            $(
                assert_token_stream!(@asserts_inner $lexer tok_pos $v);
            )*

            assert_eq!($lexer.next(), None);

            assert_eq!(tok_pos, $source.len());
        };

        (@asserts_inner $lexer:ident $tok_pos:ident ($tok:ident: $s:literal)) => {
            assert_eq!(
                ($lexer).next(),
                Some(Ok(($tok_pos, Token { tk: Tk::$tok, input: $s }, $tok_pos + $s.len())))
            );

            $tok_pos += $s.len();
        };

        (@asserts_inner $lexer:ident $tok_pos:ident (@hidden $s:literal)) => {
            $tok_pos += $s.len();
        };
    }

    #[test]
    fn no_newlines_in_brackets() {
        assert_token_stream!(
            "(\n)\n[\n]",
            [
                (OpenParen: "("),
                (@hidden "\n"),
                (CloseParen: ")"),
                (Newline: "\n"),
                (OpenBracket: "["),
                (@hidden "\n"),
                (CloseBracket: "]"),
            ]
        );
    }

    #[test]
    fn continuation() {
        assert_token_stream!(
            "\n  a",
            [
                (NewlineContinuation: "\n  "),
                (Name: "a"),
            ]
        );
    }

    #[test]
    fn continuation_tower() {
        assert_token_stream!(
            "\n  a\n    b\n      c",
            [
                (NewlineContinuation: "\n  "),
                (Name: "a"),
                (NewlineContinuation: "\n    "),
                (Name: "b"),
                (NewlineContinuation: "\n      "),
                (Name: "c"),
            ]
        );
    }

    #[test]
    fn continuation_chain() {
        assert_token_stream!(
            "x = 1 +\n  a\n  b\n  c\n  d",
            [
                (Name: "x"),
                (@hidden " "),
                (Eq: "="),
                (@hidden " "),
                (DecNum: "1"),
                (@hidden " "),
                (Plus: "+"),
                (NewlineContinuation: "\n  "),
                (Name: "a"),
                (NewlineContinuation: "\n  "),
                (Name: "b"),
                (NewlineContinuation: "\n  "),
                (Name: "c"),
                (NewlineContinuation: "\n  "),
                (Name: "d"),
            ]
        );
    }

    #[test]
    fn newline_chain() {
        assert_token_stream!(
            "\n\n\n",
            [
                (Newline: "\n"),
                (Newline: "\n"),
                (Newline: "\n"),
            ]
        );
    }

    #[test]
    fn no_continuation_after_brace() {
        assert_token_stream!(
            "{\n  a\n  b\n  c\n  d\n}",
            [
                (OpenBrace: "{"),
                (Newline: "\n"),
                (Whitespace: "  "),
                (Name: "a"),
                (Newline: "\n"),
                (Whitespace: "  "),
                (Name: "b"),
                (Newline: "\n"),
                (Whitespace: "  "),
                (Name: "c"),
                (Newline: "\n"),
                (Whitespace: "  "),
                (Name: "d"),
                (Newline: "\n"),
                (CloseBrace: "}"),
            ]
        );
    }

    #[test]
    fn no_continuation_after_brace2() {
        assert_token_stream!(
            "{\n  {\n    a\n    b\n    c\n    d\n  }\n}",
            [
                (OpenBrace: "{"),
                (Newline: "\n"),
                (Whitespace: "  "),
                (OpenBrace: "{"),
                (Newline: "\n"),
                (Whitespace: "    "),
                (Name: "a"),
                (Newline: "\n"),
                (Whitespace: "    "),
                (Name: "b"),
                (Newline: "\n"),
                (Whitespace: "    "),
                (Name: "c"),
                (Newline: "\n"),
                (Whitespace: "    "),
                (Name: "d"),
                (Newline: "\n"),
                (Whitespace: "  "),
                (CloseBrace: "}"),
                (Newline: "\n"),
                (CloseBrace: "}"),
            ]
        );
    }

    #[test]
    fn no_continuation_after_empty_line() {
        assert_token_stream!(
            "{\n  a\n\n  b\n}",
            [
                (OpenBrace: "{"),
                (Newline: "\n"),
                (Whitespace: "  "),
                (Name: "a"),
                (Newline: "\n"),
                (Newline: "\n"),
                (Whitespace: "  "),
                (Name: "b"),
                (Newline: "\n"),
                (CloseBrace: "}"),
            ]
        );
    }
}
