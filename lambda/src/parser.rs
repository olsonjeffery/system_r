use crate::context::Context;
use crate::lexer::{Lexer, Token};
use std::iter::Peekable;
use std::ops::Deref;
use std::rc::Rc;
use util::diagnostic::Diagnostic;
use util::span::*;

#[derive(Clone, PartialEq, PartialOrd)]
pub struct RcTerm(pub Rc<Term>);

// impl From<Term> for RcTerm {
//     fn from(tm: Term) -> Self {
//         RcTerm(Rc::new(tm))
//     }
// }

impl std::fmt::Debug for RcTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl std::fmt::Debug for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Term::TmVar(_, v) => write!(f, "{}", v),
            Term::TmAbs(_, tm) => write!(f, "λ.{:?}", tm),
            Term::TmApp(_, t, b) => write!(f, "{:?} {:?}", t, b),
        }
    }
}

impl Into<RcTerm> for Term {
    fn into(self) -> RcTerm {
        RcTerm(Rc::new(self))
    }
}

impl Deref for RcTerm {
    type Target = Term;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
pub enum Term {
    TmVar(Span, usize),
    TmAbs(Span, RcTerm),
    TmApp(Span, RcTerm, RcTerm),
}

impl Term {
    fn span(&self) -> Span {
        match self {
            Term::TmVar(sp, _) => *sp,
            Term::TmAbs(sp, _) => *sp,
            Term::TmApp(sp, _, _) => *sp,
        }
    }

    pub fn pretty(&self, ctx: &Context) {}
}

pub struct Parser<'s> {
    ctx: Context,
    diagnostic: Diagnostic<'s>,
    /// [`Lexer`] impls [`Iterator`] over [`TokenSpan`],
    /// so we can just directly wrap it in a [`Peekable`]
    lexer: Peekable<Lexer<'s>>,
    span: Span,
}

impl<'s> Parser<'s> {
    /// Create a new [`Parser`] for the input `&str`
    pub fn new(input: &'s str) -> Parser<'s> {
        Parser {
            ctx: Context::default(),
            diagnostic: Diagnostic::new(input),
            lexer: Lexer::new(input.chars()).peekable(),
            span: Span::default(),
        }
    }

    /// Return the last parsing error as a formatted message, if it exists
    pub fn last_error(&mut self) -> Option<String> {
        self.diagnostic.pop()
    }

    fn consume(&mut self) -> Option<Spanned<Token>> {
        let ts = self.lexer.next()?;
        self.span = ts.span;
        Some(ts)
    }

    fn expect(&mut self, token: Token) -> Option<Spanned<Token>> {
        let spanned = self.consume()?;
        match spanned.data {
            t if t == token => Some(spanned),
            t => {
                self.diagnostic.push(
                    format!("Expected token {:?}, found {:?}", token, t),
                    spanned.span,
                );
                None
            }
        }
    }

    fn peek(&mut self) -> Option<Token> {
        self.lexer.peek().map(|s| s.data)
    }

    fn lambda(&mut self) -> Option<Term> {
        let start = self.expect(Token::Lambda)?.span;

        let var = self.consume()?;

        // Bind variable into a new context before parsing the body
        // of the lambda abstraction
        let prev_ctx = self.ctx.clone();
        let (ctx, var) = match var.data {
            Token::Var(ch) => {
                let (ctx, idx) = self.ctx.bind(format!("{}", ch));
                (ctx, Term::TmVar(var.span, idx))
            }
            x => {
                self.diagnostic
                    .push(format!("Expected variable, found {:?}", x), var.span);
                return None;
            }
        };

        self.ctx = ctx;

        let _ = self.expect(Token::Dot)?;
        let body = dbg!(self.term()?);
        let end = self.span;

        // Return to previous context
        self.ctx = prev_ctx;
        dbg!(Some(Term::TmAbs(start + end, body.into())))
    }

    fn term(&mut self) -> Option<Term> {
        match self.peek()? {
            Token::Lambda => self.lambda(),
            _ => self.application(),
        }
    }

    /// Parse an application of form:
    /// application = atom application' | atom
    /// application' = atom application' | empty
    fn application(&mut self) -> Option<Term> {
        let mut lhs = dbg!(self.atom()?);
        let span = self.span;
        while let Some(rhs) = dbg!(self.atom()) {
            lhs = Term::TmApp(span + self.span, lhs.into(), rhs.into());
        }
        dbg!(Some(lhs))
    }

    /// Parse an atomic term
    /// LPAREN term RPAREN | var
    fn atom(&mut self) -> Option<Term> {
        match self.peek()? {
            Token::LParen => {
                self.expect(Token::LParen)?;
                let term = dbg!(self.term()?);
                self.expect(Token::RParen)?;
                Some(term)
            }
            Token::Var(ch) => {
                let sp = self.consume()?.span;
                match self.ctx.lookup(format!("{}", ch)) {
                    Some(idx) => Some(Term::TmVar(sp, idx)),
                    None => {
                        self.diagnostic.push(format!("Unbound variable {}", ch), sp);
                        None
                    }
                }
            }

            _ => None,
        }
    }

    pub fn parse_term(&mut self) -> Option<Term> {
        self.term()
    }

    pub fn diagnostic(self) -> Diagnostic<'s> {
        self.diagnostic
    }
}
