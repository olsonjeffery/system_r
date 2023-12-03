use super::{Token, TokenKind};
use crate::dialect::SystemRDialect;
use crate::dialect::SystemRExtension;
use crate::system_r_util::span::{Location, Span};
use std::char;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Clone, Debug)]
pub struct Lexer<'s, TExtDialect: SystemRDialect> {
    input: Peekable<Chars<'s>>,
    current: Location,
    _token: TExtDialect::TokenKind,
    _kind: TExtDialect::Kind,
    _pat: TExtDialect::Pattern,
}

impl<'s, TExtDialect: SystemRDialect> Default for Lexer<'s, TExtDialect> {
    fn default() -> Self {
        Self {
            input: "".chars().peekable(),
            current: Default::default(),
            _token: Default::default(),
            _kind: Default::default(),
            _pat: Default::default(),
        }
    }
}

fn is_tag(x: char) -> bool {
    if x == '@' {
        return true;
    }
    false
}

impl<'s, TExtDialect: SystemRDialect> Lexer<'s, TExtDialect> {
    pub fn new(input: Chars<'s>) -> Lexer<'s, TExtDialect> {
        Lexer {
            input: input.peekable(),
            current: Location {
                line: 0,
                col: 0,
                abs: 0,
            },
            _token: Default::default(),
            _kind: Default::default(),
            _pat: Default::default(),
        }
    }

    #[allow(dead_code)]
    fn rest(&mut self) -> String {
        let rest = self.input.clone().take(usize::MAX).collect();
        println!("remaining content in lexer buffer: {}", rest);
        rest
    }

    /// Peek at the next [`char`] in the input stream
    fn peek(&mut self) -> Option<char> {
        self.input.peek().cloned()
    }

    /// Consume the next [`char`] and advance internal source position
    fn consume(&mut self) -> Option<char> {
        match self.input.next() {
            Some('\n') => {
                self.current.line += 1;
                self.current.col = 0;
                self.current.abs += 1;
                Some('\n')
            }
            Some(ch) => {
                self.current.col += 1;
                self.current.abs += 1;
                Some(ch)
            }
            None => None,
        }
    }

    /// Consume characters from the input stream while pred(peek()) is true,
    /// collecting the characters into a string.
    fn consume_while<F: Fn(char) -> bool>(&mut self, pred: F) -> (String, Span) {
        let mut s = String::new();
        let start = self.current;
        while let Some(n) = self.peek() {
            if pred(n) {
                match self.consume() {
                    Some(ch) => s.push(ch),
                    None => break,
                }
            } else {
                break;
            }
        }
        (s, Span::new(start, self.current))
    }

    /// Eat whitespace
    fn consume_delimiter(&mut self) {
        let _ = self.consume_while(char::is_whitespace);
    }

    /// Lex a natural number
    fn number(&mut self) -> Token<TExtDialect::TokenKind> {
        // Since we peeked at least one numeric char, we should always
        // have a string containing at least 1 single digit, as such
        // it is safe to call unwrap() on str::parse<u32>
        let (data, span) = self.consume_while(char::is_numeric);
        let n = data.parse::<u64>().unwrap();
        Token::new(TokenKind::Nat(n), span)
    }

    fn tag(&mut self) -> Token<TExtDialect::TokenKind> {
        let (data, span) = self.consume_while(|ch| is_tag(ch) || ch.is_ascii_alphanumeric());
        let kind = TokenKind::Tag(data);
        Token::new(kind, span)
    }

    fn extended_single<TExt: SystemRExtension<TExtDialect>>(
        &mut self,
        ext: &mut TExt,
    ) -> Token<TExtDialect::TokenKind> {
        let (data, span) = self.consume_while(|ch| ext.lex_is_extended_single_pred(ch));
        let kind = ext.lex_extended_single(&data);
        Token::new(TokenKind::Extended(kind), span)
    }

    /// Lex a reserved keyword or an identifier
    fn keyword<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Token<TExtDialect::TokenKind> {
        let (data, span) = self.consume_while(|ch| ch.is_ascii_alphanumeric());
        let kind = match data.as_ref() {
            data if ext.lex_is_ext_keyword(data) => {
                let output = match ext.lex_ext_keyword(data) {
                    Ok(t) => t,
                    Err(e) => panic!("lexer: got err result from lex_ext_keyword: {:?}", e)
                };
                TokenKind::Extended(output)
            },
            "if" => TokenKind::If,
            "then" => TokenKind::Then,
            "else" => TokenKind::Else,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "succ" => TokenKind::Succ,
            "pred" => TokenKind::Pred,
            "iszero" => TokenKind::IsZero,
            "zero" => TokenKind::Nat(0),
            "Bool" => TokenKind::TyBool,
            "Nat" => TokenKind::TyNat,
            "Unit" => TokenKind::TyUnit,
            "unit" => TokenKind::Unit,
            "let" => TokenKind::Let,
            "in" => TokenKind::In,
            "fix" => TokenKind::Fix,
            "case" => TokenKind::Case,
            "of" => TokenKind::Of,
            "fold" => TokenKind::Fold,
            "unfold" => TokenKind::Unfold,
            "rec" => TokenKind::Rec,
            "lambda" => TokenKind::Lambda,
            "forall" => TokenKind::Forall,
            "exists" => TokenKind::Exists,
            "pack" => TokenKind::Pack,
            "unpack" => TokenKind::Unpack,
            "as" => TokenKind::As,

            _ => {
                if data.starts_with(|ch: char| ch.is_ascii_uppercase()) {
                    TokenKind::Uppercase(data)
                } else {
                    TokenKind::Lowercase(data)
                }
            }
        };
        Token::new(kind, span)
    }

    /// Consume the next input character, expecting to match `ch`.
    /// Return a [`ExtTokenKind::Invalid`] if the next character does not match,
    /// or the argument `kind` if it does
    fn eat(&mut self, ch: char, kind: TokenKind<TExtDialect::TokenKind>) -> Token<TExtDialect::TokenKind> {
        let loc = self.current;
        // Lexer::eat() should only be called internally after calling peek()
        // so we know that it's safe to unwrap the result of Lexer::consume()
        let n = self.consume().unwrap();
        let kind = if n == ch { kind } else { TokenKind::Invalid(n) };
        Token::new(kind, Span::new(loc, self.current))
    }

    /// Return the next lexeme in the input as a [`Token`]
    pub fn lex<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Token<TExtDialect::TokenKind> {
        self.consume_delimiter();
        let next = match self.peek() {
            Some(ch) => ch,
            None => return Token::new(TokenKind::Eof, Span::new(self.current, self.current)),
        };
        match next {
            x if ext.lex_is_ext_single(x) => self.extended_single(ext),
            x if is_tag(x) => self.tag(),
            x if x.is_ascii_alphabetic() => self.keyword(ext),
            x if x.is_numeric() => self.number(),
            '(' => self.eat('(', TokenKind::LParen),
            ')' => self.eat(')', TokenKind::RParen),
            ';' => self.eat(';', TokenKind::Semicolon),
            ':' => self.eat(':', TokenKind::Colon),
            ',' => self.eat(',', TokenKind::Comma),
            '{' => self.eat('{', TokenKind::LBrace),
            '}' => self.eat('}', TokenKind::RBrace),
            '[' => self.eat('[', TokenKind::LSquare),
            ']' => self.eat(']', TokenKind::RSquare),
            '\\' => self.eat('\\', TokenKind::Lambda),
            'λ' => self.eat('λ', TokenKind::Lambda),
            '∀' => self.eat('∀', TokenKind::Forall),
            '∃' => self.eat('∃', TokenKind::Exists),
            '.' => self.eat('.', TokenKind::Proj),
            '=' => self.eat('=', TokenKind::Equals),
            '|' => self.eat('|', TokenKind::Bar),
            '_' => self.eat('_', TokenKind::Wildcard),
            '>' => self.eat('>', TokenKind::Gt),
            '-' => {
                self.consume();
                self.eat('>', TokenKind::TyArrow)
            }
            ch => self.eat(' ', TokenKind::Invalid(ch)),
        }
    }
}

impl<'s, TExtDialect: SystemRDialect> Lexer<'s, TExtDialect> {
    /// FIXME Choosing to keep this, because if/when a move
    /// to a flat AST occurs, this will be handy
    #[allow(dead_code)]
    fn next<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Option<Token<TExtDialect::TokenKind>> {
        match self.lex(ext) {
            Token {
                kind: TokenKind::Eof, ..
            } => None,
            tok => Some(tok),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::bottom::{BottomDialect, BottomExtension, BottomTokenKind};

    use super::*;
    use TokenKind::*;

    fn get_tokens_from<'s>(input: &'s str) -> Vec<TokenKind<BottomTokenKind>> {
        let mut stub_ext = BottomExtension;
        let mut ret_val = Vec::new();
        let mut lexer: Lexer<'s, BottomDialect> = Lexer::new(input.chars());

        let mut next_token = lexer.next(&mut stub_ext);
        while next_token.is_some() {
            ret_val.push(next_token.unwrap().kind);
            next_token = lexer.next(&mut stub_ext);
        }

        ret_val
    }

    #[test]
    fn nested() {
        let input = "succ(succ(succ(0)))";

        let expected = vec![Succ, LParen, Succ, LParen, Succ, LParen, Nat(0), RParen, RParen, RParen];
        let output = get_tokens_from(input);
        assert_eq!(expected, output);
    }

    #[test]
    fn case() {
        let input = "case x of | A _ => true | B x => (\\y: Nat. x)";
        let expected = vec![
            Case,
            Lowercase("x".into()),
            Of,
            Bar,
            Uppercase("A".into()),
            Wildcard,
            Equals,
            Gt,
            True,
            Bar,
            Uppercase("B".into()),
            Lowercase("x".into()),
            Equals,
            Gt,
            LParen,
            Lambda,
            Lowercase("y".into()),
            Colon,
            TyNat,
            Proj,
            Lowercase("x".into()),
            RParen,
        ];
        let output = get_tokens_from(input);
        assert_eq!(expected, output);
    }
}
