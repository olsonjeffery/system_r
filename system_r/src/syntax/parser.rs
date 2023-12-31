use super::debruijn::DeBruijnIndexer;
use super::lexer::Lexer;
use super::{Token, TokenKind};
use crate::bottom::{BottomDialect, BottomExtension};
use crate::dialect::{SystemRDialect, SystemRExtension};
use crate::feedback::{catalog, ErrorKind};

use crate::util::span::*;
use core::fmt;

use crate::patterns::{PatVarStack, Pattern};
use crate::platform_bindings::Bindings;
use crate::terms::*;
use crate::type_check::*;

use anyhow::Result;

#[derive(Default, Debug, Clone)]
pub struct Parser<'s, TExtDialect: SystemRDialect + 'static> {
    pub tmvar: DeBruijnIndexer,
    pub tyvar: DeBruijnIndexer,
    pub lexer: Lexer<'s, TExtDialect>,
    pub span: Span,
    pub token: Token<TExtDialect::TokenKind>,
    pub platform_bindings: Bindings,
    pub ext_state: TExtDialect::DialectState,
    pub ty: TExtDialect::Type,
}

impl<'s, TExtDialect: SystemRDialect + 'static> Parser<'s, TExtDialect> {
    pub fn to_ext_state(self) -> TExtDialect::DialectState {
        self.ext_state
    }

    pub fn new(platform_bindings: &'s Bindings, input: &'s str) -> Parser<'s, BottomDialect> {
        let mut ext = BottomExtension;
        Parser::ext_new(platform_bindings, input, &mut ext)
    }

    pub fn ext_new<TExt: SystemRExtension<TExtDialect>>(
        platform_bindings: &'s Bindings,
        input: &'s str,
        ext: &mut TExt,
    ) -> Parser<'s, TExtDialect> {
        let mut p = Parser {
            tmvar: DeBruijnIndexer::default(),
            tyvar: DeBruijnIndexer::default(),
            lexer: Lexer::new(input.chars()),
            span: Span::default(),
            token: Token::dummy(),
            platform_bindings: platform_bindings.clone(),
            ext_state: Default::default(),
            ty: Default::default(),
        };
        p.bump(ext);
        p
    }

    /// Kleene Plus combinator
    pub fn once_or_more<F: FnMut(&mut Self) -> Result<T>, TExt: SystemRExtension<TExtDialect>, T>(
        &mut self,
        mut func: F,
        delimiter: TokenKind<TExtDialect::TokenKind>,
        ext: &mut TExt,
    ) -> Result<Vec<T>> {
        let mut v = vec![func(self)?];
        while self.bump_if(ext, &delimiter) {
            v.push(func(self)?);
        }
        Ok(v)
    }

    /// Expect combinator
    /// Combinator that must return Ok or a message will be pushed to
    /// diagnostic. This method should only be called after a token has
    /// already been bumped.
    pub fn once<F: FnMut(&mut Self) -> Result<T>, T>(&mut self, mut func: F, message: &str) -> Result<T> {
        match func(self) {
            Err(e) => Err(catalog::syntax::err_05_once_failure::<TExtDialect>(
                &self.span,
                &self.token,
                message.to_owned(),
                format!("{:?}", e),
            )
            .into()),
            t => t,
        }
    }

    pub fn bump<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> TokenKind<TExtDialect::TokenKind> {
        let prev = std::mem::replace(&mut self.token, self.lexer.lex(ext));
        self.span = prev.span;
        prev.kind
    }

    /// Used within parser-logic to advance the ParserState to the next token,
    /// if the current token's kind matches the given `kind` arg and returning
    /// true, otherwise returns false
    pub fn bump_if<TExt: SystemRExtension<TExtDialect>>(
        &mut self,
        ext: &mut TExt,
        kind: &TokenKind<TExtDialect::TokenKind>,
    ) -> bool {
        if &self.token.kind == kind {
            self.bump(ext);
            true
        } else {
            false
        }
    }

    /// With the provided ParserState + TExt + a given TokenKind, return Err
    /// Result if current token kind is NOT eq to the given `kind` arg,
    /// otherwise returns an Ok(()) unit result
    pub fn expect<TExt: SystemRExtension<TExtDialect>>(
        &mut self,
        ext: &mut TExt,
        kind: TokenKind<TExtDialect::TokenKind>,
    ) -> Result<()>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        if self.token.kind == kind {
            self.bump(ext);
            Ok(())
        } else {
            Err(catalog::syntax::err_06_expected_token::<TExtDialect>(&self.span, &self.token, &kind).into())
        }
    }

    pub fn error<TError>(&self, kind: ErrorKind<TExtDialect::TokenKind>) -> Result<TError>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        Err(catalog::syntax::err_02::<TExtDialect>(self.span, self.token.clone(), kind).into())
    }

    pub fn ty_variant<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Variant<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        let label = self.uppercase_id(ext)?;
        let ty = match self.ty(ext) {
            Ok(ty) => ty,
            _ => Type::Unit,
        };

        Ok(Variant { label, ty })
    }

    pub fn ty_app<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Type<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        if !self.bump_if(ext, &TokenKind::LSquare) {
            return self.error(ErrorKind::ExpectedToken(TokenKind::LSquare));
        }
        let ty = self.ty(ext)?;
        self.expect(ext, TokenKind::RSquare)?;
        Ok(ty)
    }

    pub fn ty_atom<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Type<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        match self.token.kind.clone() {
            TokenKind::Tag(s) => {
                let v = s.to_owned();
                self.bump(ext);
                Ok(Type::Tag(v))
            }
            TokenKind::TyBool => {
                self.bump(ext);
                Ok(Type::Bool)
            }
            TokenKind::TyNat => {
                self.bump(ext);
                Ok(Type::Nat)
            }
            TokenKind::TyUnit => {
                self.bump(ext);
                Ok(Type::Unit)
            }
            TokenKind::LParen => {
                self.bump(ext);
                let r = self.ty(ext)?;
                self.expect(ext, TokenKind::RParen)?;
                Ok(r)
            }
            TokenKind::Forall => {
                self.bump(ext);
                Ok(Type::Universal(Box::new(self.ty(ext)?)))
            }
            TokenKind::Exists => {
                self.bump(ext);
                let tvar = self.uppercase_id(ext)?;
                self.expect(ext, TokenKind::Proj)?;
                self.tyvar.push(tvar);
                let xs = Type::Existential(Box::new(self.ty(ext)?));
                self.tyvar.pop();
                Ok(xs)
            }
            TokenKind::Uppercase(_) => {
                let ty = self.uppercase_id(ext)?;
                match self.tyvar.lookup(&ty) {
                    Some(idx) => Ok(Type::Var(idx)),
                    None => Ok(Type::Alias(ty)),
                }
            }
            TokenKind::LBrace => {
                self.bump(ext);
                let mut ext2 = *ext;
                let mut ext3 = *ext;
                let fields = self.once_or_more(|p| p.ty_variant(&mut ext2), TokenKind::Bar, &mut ext3)?;
                self.expect(ext, TokenKind::RBrace)?;
                Ok(Type::Variant(fields))
            }
            v => self.error(ErrorKind::ExtendedError(format!(
                "Expected type-able token, got {:?}",
                v
            ))),
        }
    }

    pub fn ty_tuple<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Type<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        if self.bump_if(ext, &TokenKind::LParen) {
            let mut ext2 = *ext;
            let mut ext3 = *ext;
            let mut v = self.once_or_more(|p| p.ty(&mut ext2), TokenKind::Comma, &mut ext3)?;
            self.expect(ext, TokenKind::RParen)?;

            if v.len() > 1 {
                Ok(Type::Product(v))
            } else {
                Ok(v.remove(0))
            }
        } else {
            self.ty_atom(ext)
        }
    }

    pub fn ty<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Type<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        if ext.parser_ty_bump_if(self) {
            return ext.parser_ty(self);
        }

        if self.bump_if(ext, &TokenKind::Rec) {
            let name = self.uppercase_id(ext)?;
            self.expect(ext, TokenKind::Equals)?;
            self.tyvar.push(name);
            let ty = self.ty(ext)?;
            self.tyvar.pop();
            return Ok(Type::Rec(Box::new(ty)));
        }

        let mut lhs = self.ty_tuple(ext)?;
        if let TokenKind::TyArrow = self.token.kind {
            self.bump(ext);
            while let Ok(rhs) = self.ty(ext) {
                lhs = Type::Arrow(Box::new(lhs), Box::new(rhs));
                if let TokenKind::TyArrow = self.token.kind {
                    self.bump(ext);
                } else {
                    break;
                }
            }
        }
        Ok(lhs)
    }

    pub fn tyabs<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        let tyvar = self.uppercase_id(ext)?;
        let sp = self.span;
        let ty: Box<Type<TExtDialect>> = Box::new(Type::Var(self.tyvar.push(tyvar)));
        let body = self.once(|p| p.parse(ext), "abstraction body required")?;
        Ok(Term::new(Kind::TyAbs(Box::new(body)), sp + self.span))
    }

    pub fn tmabs<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        let tmvar = self.lowercase_id(ext)?;
        let sp = self.span;
        self.tmvar.push(tmvar);

        self.expect(ext, TokenKind::Colon)?;
        let ty = self.once(|p| p.ty(ext), "type annotation required in abstraction")?;
        self.expect(ext, TokenKind::Proj)?;
        let body = self.once(|p| p.parse(ext), "abstraction body required")?;
        self.tmvar.pop();
        Ok(Term::new(Kind::Abs(Box::new(ty), Box::new(body)), sp + self.span))
    }

    pub fn fold<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        self.expect(ext, TokenKind::Fold)?;
        let sp = self.span;
        let ty = self.once(|p| p.ty(ext), "type annotation required after `fold`")?;
        let tm = self.once(|p| p.parse(ext), "term required after `fold`")?;
        Ok(Term::new(Kind::Fold(Box::new(ty), Box::new(tm)), sp + self.span))
    }

    pub fn unfold<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        self.expect(ext, TokenKind::Unfold)?;
        let sp = self.span;
        let ty = self.once(|p| p.ty(ext), "type annotation required after `unfold`")?;
        let tm = self.once(|p| p.parse(ext), "term required after `unfold`")?;
        Ok(Term::new(Kind::Unfold(Box::new(ty), Box::new(tm)), sp + self.span))
    }

    pub fn fix<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        let sp = self.span;
        self.expect(ext, TokenKind::Fix)?;
        let t = self.parse(ext)?;
        Ok(Term::new(Kind::Fix(Box::new(t)), sp + self.span))
    }

    pub fn letexpr<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        let sp = self.span;
        self.expect(ext, TokenKind::Let)?;
        let pat = self.once(|p| p.pattern(ext), "missing pattern")?;

        self.expect(ext, TokenKind::Equals)?;

        let t1 = self.once(|p| p.parse(ext), "let binder required")?;
        let len = self.tmvar.len();
        for var in PatVarStack::<TExtDialect>::collect(&pat, ext, &self.ext_state)
            .into_iter()
            .rev()
        {
            self.tmvar.push(var);
        }
        self.expect(ext, TokenKind::In)?;
        let t2 = self.once(|p| p.parse(ext), "let body required")?;
        while self.tmvar.len() > len {
            self.tmvar.pop();
        }
        Ok(Term::new(
            Kind::Let(Box::new(pat), Box::new(t1), Box::new(t2)),
            sp + self.span,
        ))
    }

    pub fn lambda<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        self.expect(ext, TokenKind::Lambda)?;
        match &self.token.kind {
            TokenKind::Uppercase(_) => self.tyabs(ext),
            TokenKind::Lowercase(_) => self.tmabs(ext),
            t => Err(catalog::syntax::err_07::<TExtDialect>(&self.span, &self.token, t).into()),
        }
    }

    pub fn paren<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        self.expect(ext, TokenKind::LParen)?;
        let span = self.span;

        let mut ext2 = *ext;
        let mut ext3 = *ext;
        let mut n = self.once_or_more(|p| p.parse(&mut ext2), TokenKind::Comma, &mut ext3)?;
        self.expect(ext, TokenKind::RParen)?;
        if n.len() > 1 {
            Ok(Term::new(Kind::Product(n), span + self.span))
        } else {
            // invariant, n.len() >= 1
            Ok(n.remove(0))
        }
    }

    pub fn uppercase_id<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<String>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        match self.bump(ext) {
            TokenKind::Uppercase(s) => Ok(s),
            tk => Err(catalog::syntax::err_08::<TExtDialect>(&self.span, &self.token, &tk, true).into()),
        }
    }

    pub fn lowercase_id<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<String>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        match self.bump(ext) {
            TokenKind::Lowercase(s) => Ok(s),
            tk => Err(catalog::syntax::err_08::<TExtDialect>(&self.span, &self.token, &tk, false).into()),
        }
    }

    pub fn literal<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        let lit = match self.bump(ext) {
            TokenKind::Nat(x) => Literal::Nat(x),
            TokenKind::True => Literal::Bool(true),
            TokenKind::False => Literal::Bool(false),
            TokenKind::Unit => Literal::Unit,
            TokenKind::Tag(s) => Literal::Tag(s),
            _ => return self.error(ErrorKind::Unknown),
        };
        Ok(Term::new(Kind::Lit(lit), self.span))
    }

    pub fn primitive<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        let p = match self.bump(ext) {
            TokenKind::IsZero => Primitive::IsZero,
            TokenKind::Succ => Primitive::Succ,
            TokenKind::Pred => Primitive::Pred,
            _ => return self.error(ErrorKind::Unknown),
        };
        Ok(Term::new(Kind::Primitive(p), self.span))
    }

    /// Important to note that this function can push variable names to the
    /// de Bruijn naming context. Callers of this function are responsible for
    /// making sure that the stack is balanced afterwards
    pub fn pat_atom<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Pattern<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        match self.token.kind.clone() {
            TokenKind::LParen => self.pattern(ext),
            TokenKind::Wildcard => {
                self.bump(ext);
                Ok(Pattern::Any)
            }
            TokenKind::Uppercase(_) => {
                let tycon = self.uppercase_id(ext)?;
                let inner = match self.pattern(ext) {
                    Ok(pat) => pat,
                    _ => Pattern::Any,
                };
                Ok(Pattern::Constructor(tycon, Box::new(inner)))
            }
            TokenKind::Lowercase(_) => {
                let var = self.lowercase_id(ext)?;
                // self.tmvar.push(var.clone());
                Ok(Pattern::Variable(var))
            }
            TokenKind::True => {
                self.bump(ext);
                Ok(Pattern::Literal(Literal::Bool(true)))
            }
            TokenKind::False => {
                self.bump(ext);
                Ok(Pattern::Literal(Literal::Bool(false)))
            }
            TokenKind::Unit => {
                self.bump(ext);
                Ok(Pattern::Literal(Literal::Unit))
            }
            TokenKind::Nat(n) => {
                // O great borrowck, may this humble offering appease thee
                self.bump(ext);
                Ok(Pattern::Literal(Literal::Nat(n)))
            }
            TokenKind::Tag(s) => {
                let s = s.clone();
                self.bump(ext);
                Ok(Pattern::Literal(Literal::Tag(s)))
            }
            _ => self.error(ErrorKind::ExpectedPattern),
        }
    }

    pub fn pattern<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Pattern<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        match self.token.kind {
            TokenKind::LParen => {
                self.bump(ext);
                let mut ext2 = *ext;
                let mut ext3 = *ext;
                let mut v = self.once_or_more(|p| p.pat_atom(&mut ext2), TokenKind::Comma, &mut ext3)?;
                self.expect(ext, TokenKind::RParen)?;
                if v.len() > 1 {
                    Ok(Pattern::Product(v))
                } else {
                    // v must have length == 1, else we would have early returned
                    assert_eq!(v.len(), 1);
                    Ok(v.remove(0))
                }
            }
            _ => self.pat_atom(ext),
        }
    }

    pub fn case_arm<TExt: fmt::Debug + SystemRExtension<TExtDialect>>(
        &mut self,
        ext: &mut TExt,
    ) -> Result<Arm<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        // We don't track the length of the debruijn index in other methods,
        // but we have a couple branches where variables might be bound,
        // and this is pretty much the easiest way of doing it

        let len = self.tmvar.len();
        let mut span = self.span;

        let pat = self.once(|p| p.pattern(ext), "missing pattern")?;

        for var in PatVarStack::<TExtDialect>::collect(&pat, ext, &self.ext_state)
            .into_iter()
            .rev()
        {
            self.tmvar.push(var);
        }

        self.expect(ext, TokenKind::Equals)?;
        self.expect(ext, TokenKind::Gt)?;

        let term = Box::new(self.once(|p| p.application(ext), "missing case term")?);

        self.bump_if(ext, &TokenKind::Comma);

        // Unbind any variables from the parsing context
        while self.tmvar.len() > len {
            self.tmvar.pop();
        }

        span += self.span;

        Ok(Arm { span, pat, term })
    }

    pub fn case<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        self.expect(ext, TokenKind::Case)?;
        let span = self.span;
        let expr = self.once(|p| p.parse(ext), "missing case expression")?;
        self.expect(ext, TokenKind::Of)?;

        self.bump_if(ext, &TokenKind::Bar);

        let mut ext2 = *ext;
        let mut ext3 = *ext;
        let arms = self.once_or_more(|p| p.case_arm(&mut ext3), TokenKind::Bar, &mut ext2)?;

        Ok(Term::new(Kind::Case(Box::new(expr), arms), span + self.span))
    }

    pub fn injection<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        let label = self.uppercase_id(ext)?;
        let sp = self.span;
        let term = match self.parse(ext) {
            Ok(t) => t,
            _ => Term::new(Kind::Lit(Literal::Unit), self.span),
        };

        self.expect(ext, TokenKind::Of)?;
        let ty = self.ty(ext)?;
        Ok(Term::new(
            Kind::Injection(label, Box::new(term), Box::new(ty)),
            sp + self.span,
        ))
    }

    pub fn pack<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        self.expect(ext, TokenKind::Pack)?;
        let sp = self.span;
        let witness = self.ty(ext)?;
        self.expect(ext, TokenKind::Comma)?;
        let evidence = self.parse(ext)?;
        self.expect(ext, TokenKind::As)?;
        let signature = self.ty(ext)?;

        Ok(Term::new(
            Kind::Pack(Box::new(witness), Box::new(evidence), Box::new(signature)),
            sp + self.span,
        ))
    }

    pub fn unpack<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        self.expect(ext, TokenKind::Unpack)?;
        let sp = self.span;
        let package = self.parse(ext)?;
        self.expect(ext, TokenKind::As)?;

        let tyvar = self.uppercase_id(ext)?;
        self.expect(ext, TokenKind::Comma)?;
        let name = self.lowercase_id(ext)?;
        self.tyvar.push(tyvar);
        self.tmvar.push(name);
        self.expect(ext, TokenKind::In)?;
        let expr = self.parse(ext)?;
        self.tmvar.pop();
        self.tyvar.pop();
        Ok(Term::new(
            Kind::Unpack(Box::new(package), Box::new(expr)),
            sp + self.span,
        ))
    }

    pub fn numeric_bytes_arr<TExt: SystemRExtension<TExtDialect>>(
        &mut self,
        ext: &mut TExt,
    ) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        // advance past opening LSquare
        let start_span = self.span;
        let start_token = self.token.clone();
        self.bump(ext);

        let mut ext_2 = *ext;
        let nats = self.once_or_more(|p| p.atom(&mut ext_2), TokenKind::Comma, ext)?;
        let mut out_bytes: Vec<u8> = Vec::new();
        for n in nats {
            match n.kind {
                Kind::Lit(Literal::Nat(n)) => match n {
                    n @ 0..256 => {
                        out_bytes.push(n as u8);
                    }
                    e => {
                        return Err(catalog::syntax::err_01::<TExtDialect>(
                            start_span,
                            start_token,
                            ErrorKind::ExtendedError(
                                "expect Nat literals from 0 to 255, got {e:?}; invalid byte range".to_owned(),
                            ),
                        )
                        .into());
                    }
                },
                v => {
                    return Err(catalog::syntax::err_03(
                        start_span,
                        start_token,
                        ErrorKind::ExtendedError("".to_string()),
                        v,
                    )
                    .into())
                }
            }
        }

        // lastly we should expect a closing RSquare, no trailing comma!
        self.expect(ext, TokenKind::RSquare)?;

        let mut bytes = Term::unit();
        bytes.span = Span {
            start: start_span.start,
            end: self.span.end,
        };
        bytes.kind = Kind::Lit(Literal::Bytes(out_bytes));
        Ok(bytes)
    }

    pub fn atom<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        match self.token.kind.clone() {
            TokenKind::Extended(tk) => self.ext_atom(ext),
            TokenKind::LSquare => self.numeric_bytes_arr(ext),
            TokenKind::LParen => self.paren(ext),
            TokenKind::Fix => self.fix(ext),
            TokenKind::Fold => self.fold(ext),
            TokenKind::Unfold => self.unfold(ext),
            TokenKind::Pack => self.pack(ext),
            TokenKind::Unpack => self.unpack(ext),
            TokenKind::IsZero | TokenKind::Succ | TokenKind::Pred => self.primitive(ext),
            TokenKind::Uppercase(_) => self.injection(ext),
            TokenKind::Lowercase(s) => {
                let var = self.lowercase_id(ext)?;
                match self.tmvar.lookup(&var) {
                    Some(idx) => Ok(Term::new(Kind::Var(idx), self.span)),
                    None => {
                        if let Some(idx) = self.platform_bindings.has(&var) {
                            return Ok(Term::new(Kind::PlatformBinding(idx), self.span));
                        }
                        Err(catalog::syntax::err_09::<TExtDialect>(&self.span, &self.token, var).into())
                    }
                }
            }
            TokenKind::Nat(_) | TokenKind::True | TokenKind::False | TokenKind::Unit | TokenKind::Tag(_) => {
                self.literal(ext)
            }
            TokenKind::Eof => self.error(ErrorKind::Eof),
            TokenKind::Semicolon => {
                self.bump(ext);
                self.error(ErrorKind::ExpectedAtom)
            }
            _ => self.error(ErrorKind::ExpectedAtom),
        }
    }

    /// Parse a term of form:
    /// projection = atom `.` projection
    /// projection = atom
    pub fn projection<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        let atom = self.atom(ext)?;
        if self.bump_if(ext, &TokenKind::Proj) {
            let idx = match self.bump(ext) {
                TokenKind::Nat(idx) => idx,
                _ => return Err(catalog::syntax::err_010(&self.span, &self.token, &atom).into()),
            };
            let sp = atom.span + self.span;
            Ok(Term::new(Kind::Projection(Box::new(atom), idx as usize), sp))
        } else {
            Ok(atom)
        }
    }

    /// Parse an application of form:
    /// application = atom application' | atom
    /// application' = atom application' | empty
    pub fn application<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        let mut app = self.projection(ext)?;

        loop {
            let sp = app.span;
            if let Ok(ty) = self.ty_app(ext) {
                // Full type inference for System F is undecidable
                // Additionally, even partial type reconstruction,
                // where only type application types are erased is also
                // undecidable, see TaPL 23.6.2, Boehm 1985, 1989
                //
                // Partial erasure rules:
                // erasep(x) = x
                // erasep(λx:T. t) = λx:T. erasep(t)
                // erasep(t1 t2) = erasep(t1) erasep(t2)
                // erasep(λX. t) = λX. erasep(t)
                // erasep(t T) = erasep(t) []      <--- erasure of TyApp
                app = Term::new(Kind::TyApp(Box::new(app), Box::new(ty)), sp + self.span);
            } else if let Ok(term) = self.projection(ext) {
                app = Term::new(Kind::App(Box::new(app), Box::new(term)), sp + self.span);
            } else {
                break;
            }
        }
        Ok(app)
    }

    pub fn ext_atom<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        let r = {
            match ext.parser_ext_atom(self) {
                Err(e) => return Err(e),
                Ok(t) => t,
            }
        };
        Ok(r)
    }

    pub fn ext_parse<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        ext.parser_ext_parse(self)
    }

    pub fn parse<TExt: SystemRExtension<TExtDialect>>(&mut self, ext: &mut TExt) -> Result<Term<TExtDialect>>
    where
        <TExtDialect as SystemRDialect>::TokenKind: 'static,
    {
        match self.token.kind.clone() {
            TokenKind::Case => self.case(ext),
            TokenKind::Lambda => self.lambda(ext),
            TokenKind::Let => self.letexpr(ext),
            TokenKind::Extended(tk) if ext.parser_has_ext_parse(&tk)? => self.ext_parse(ext),
            _ => self.application(ext),
        }
    }
}
