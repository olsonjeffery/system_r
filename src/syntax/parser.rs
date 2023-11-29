use super::debruijn::DeBruijnIndexer;
use super::error::Error;
use super::lexer::Lexer;
use super::{Token, TokenKind};
use crate::bottom::{BottomDialect, BottomExtension};
use crate::dialect::{ExtendedTokenKind, SystemRDialect, SystemRExtension};

use crate::system_r_util::diagnostic::Diagnostic;
use crate::system_r_util::span::*;
use core::fmt;

use crate::patterns::{PatVarStack, Pattern};
use crate::platform_bindings::PlatformBindings;
use crate::terms::*;
use crate::type_check::*;

use anyhow::Result;

#[derive(Default, Debug, Clone)]
pub struct ParserState<'s, TExtDialect: SystemRDialect> {
    pub tmvar: DeBruijnIndexer,
    pub tyvar: DeBruijnIndexer,
    pub diagnostic: Diagnostic<'s>,
    pub lexer: Lexer<'s, TExtDialect>,
    pub span: Span,
    pub token: Token<TExtDialect::TokenKind>,
    pub platform_bindings: PlatformBindings,
    pub ext_state: TExtDialect::DialectState,
    pub ty: TExtDialect::Type,
}

#[derive(Clone, Debug)]
pub enum ErrorKind<T: ExtendedTokenKind> {
    ExpectedAtom,
    ExpectedIdent,
    ExpectedType,
    ExpectedPattern,
    ExpectedToken(TokenKind<T>),
    UnboundTypeVar,
    Unknown,
    Eof,
    ExtendedError(String),
}

impl<'s, TExtDialect: SystemRDialect> ParserState<'s, TExtDialect> {
    pub fn die(self) -> String {
        let d = self.diagnostic;
        d.emit()
    }

    pub fn to_ext_state(self) -> TExtDialect::DialectState {
        self.ext_state
    }
}

pub fn new<'s>(platform_bindings: &'s PlatformBindings, input: &'s str) -> ParserState<'s, BottomDialect> {
    let mut ext = BottomExtension;
    ext_new(platform_bindings, input, &mut ext)
}

pub fn ext_new<'s, TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    platform_bindings: &'s PlatformBindings,
    input: &'s str,
    ext: &mut TExt,
) -> ParserState<'s, TExtDialect> {
    let mut p = ParserState {
        tmvar: DeBruijnIndexer::default(),
        tyvar: DeBruijnIndexer::default(),
        diagnostic: Diagnostic::new(input),
        lexer: Lexer::new(input.chars()),
        span: Span::default(),
        token: Token::dummy(),
        platform_bindings: platform_bindings.clone(),
        ext_state: Default::default(),
        ty: Default::default(),
    };
    bump(&mut p, ext);
    p
}

/// Kleene Plus combinator
pub fn once_or_more<
    TExtDialect: SystemRDialect,
    F: FnMut(&mut ParserState<TExtDialect>) -> Result<T>,
    TExt: SystemRExtension<TExtDialect>,
    T,
>(
    ps: &mut ParserState<TExtDialect>,
    mut func: F,
    delimiter: TokenKind<TExtDialect::TokenKind>,
    ext: &mut TExt,
) -> Result<Vec<T>> {
    let mut v = vec![func(ps)?];
    while bump_if(ps, ext, &delimiter) {
        v.push(func(ps)?);
    }
    Ok(v)
}

/// Expect combinator
/// Combinator that must return Ok or a message will be pushed to
/// diagnostic. This method should only be called after a token has
/// already been bumped.
pub fn once<TExtDialect: SystemRDialect, F: FnMut(&mut ParserState<TExtDialect>) -> Result<T>, T>(
    ps: &mut ParserState<TExtDialect>,
    mut func: F,
    message: &str,
) -> Result<T> {
    match func(ps) {
        Err(e) => {
            ps.diagnostic.push(message, ps.span);
            Err(e)
        }
        t => t,
    }
}

pub fn diagnostic<TExtDialect: SystemRDialect>(ps: ParserState<'_, TExtDialect>) -> Diagnostic<'_> {
    ps.diagnostic
}

pub fn error<TExtDialect: SystemRDialect, TError>(
    ps: &ParserState<TExtDialect>,
    kind: ErrorKind<TExtDialect::TokenKind>,
) -> Result<TError>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    Err(Error {
        span: ps.token.span,
        tok: ps.token.clone(),
        kind,
    }
    .into())
}

pub fn bump<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> TokenKind<TExtDialect::TokenKind> {
    let prev = std::mem::replace(&mut ps.token, ps.lexer.lex(ext));
    ps.span = prev.span;
    prev.kind
}

/// Used within parser-logic to advance the ParserState to the next token,
/// if the current token's kind matches the given `kind` arg and returning
/// true, otherwise returns false
pub fn bump_if<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
    kind: &TokenKind<TExtDialect::TokenKind>,
) -> bool {
    if &ps.token.kind == kind {
        bump(ps, ext);
        true
    } else {
        false
    }
}

/// With the provided ParserState + TExt + a given TokenKind, return Err
/// Result if current token kind is NOT eq to the given `kind` arg, otherwise
/// returns an Ok(()) unit result
pub fn expect<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
    kind: TokenKind<TExtDialect::TokenKind>,
) -> Result<()>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    if ps.token.kind == kind {
        bump(ps, ext);
        Ok(())
    } else {
        ps.diagnostic
            .push(format!("expected token {:?}, found {:?}", kind, ps.token.kind), ps.span);
        error(ps, ErrorKind::ExpectedToken(kind))
    }
}

/// Return current TokenKind for the ParserState
pub fn kind<'s, TExtDialect: SystemRDialect>(
    ps: &'s ParserState<TExtDialect>,
) -> &'s TokenKind<TExtDialect::TokenKind> {
    &ps.token.kind
}

pub fn ty_variant<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Variant<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    let label = uppercase_id(ps, ext)?;
    let ty = match ty(ps, ext) {
        Ok(ty) => ty,
        _ => Type::Unit,
    };

    Ok(Variant { label, ty })
}

pub fn ty_app<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Type<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    if !bump_if(ps, ext, &TokenKind::LSquare) {
        return error(ps, ErrorKind::ExpectedToken(TokenKind::LSquare));
    }
    let ty = ty(ps, ext)?;
    expect(ps, ext, TokenKind::RSquare)?;
    Ok(ty)
}

pub fn ty_atom<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Type<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    match kind(ps) {
        TokenKind::Tag(s) => {
            let v = s.to_owned();
            bump(ps, ext);
            Ok(Type::Tag(v))
        }
        TokenKind::TyBool => {
            bump(ps, ext);
            Ok(Type::Bool)
        }
        TokenKind::TyNat => {
            bump(ps, ext);
            Ok(Type::Nat)
        }
        TokenKind::TyUnit => {
            bump(ps, ext);
            Ok(Type::Unit)
        }
        TokenKind::LParen => {
            bump(ps, ext);
            let r = ty(ps, ext)?;
            expect(ps, ext, TokenKind::RParen)?;
            Ok(r)
        }
        TokenKind::Forall => {
            bump(ps, ext);
            Ok(Type::Universal(Box::new(ty(ps, ext)?)))
        }
        TokenKind::Exists => {
            bump(ps, ext);
            let tvar = uppercase_id(ps, ext)?;
            expect(ps, ext, TokenKind::Proj)?;
            ps.tyvar.push(tvar);
            let xs = Type::Existential(Box::new(ty(ps, ext)?));
            ps.tyvar.pop();
            Ok(xs)
        }
        TokenKind::Uppercase(_) => {
            let ty = uppercase_id(ps, ext)?;
            match ps.tyvar.lookup(&ty) {
                Some(idx) => Ok(Type::Var(idx)),
                None => Ok(Type::Alias(ty)),
            }
        }
        TokenKind::LBrace => {
            bump(ps, ext);
            let mut ext2 = *ext;
            let mut ext3 = *ext;
            let fields = once_or_more(ps, |p| ty_variant(p, &mut ext2), TokenKind::Bar, &mut ext3)?;
            expect(ps, ext, TokenKind::RBrace)?;
            Ok(Type::Variant(fields))
        }
        v => error(
            ps,
            ErrorKind::ExtendedError(format!("Expected type-able token, got {:?}", v)),
        ),
    }
}

pub fn ty_tuple<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Type<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    if bump_if(ps, ext, &TokenKind::LParen) {
        let mut ext2 = *ext;
        let mut ext3 = *ext;
        let mut v = once_or_more(ps, |p| ty(p, &mut ext2), TokenKind::Comma, &mut ext3)?;
        expect(ps, ext, TokenKind::RParen)?;

        if v.len() > 1 {
            Ok(Type::Product(v))
        } else {
            Ok(v.remove(0))
        }
    } else {
        ty_atom(ps, ext)
    }
}

pub fn ty<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Type<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    if ext.parser_ty_bump_if(ps) {
        return ext.parser_ty(ps);
    }

    if bump_if(ps, ext, &TokenKind::Rec) {
        let name = uppercase_id(ps, ext)?;
        expect(ps, ext, TokenKind::Equals)?;
        ps.tyvar.push(name);
        let ty = ty(ps, ext)?;
        ps.tyvar.pop();
        return Ok(Type::Rec(Box::new(ty)));
    }

    let mut lhs = ty_tuple(ps, ext)?;
    if let TokenKind::TyArrow = kind(ps) {
        bump(ps, ext);
        while let Ok(rhs) = ty(ps, ext) {
            lhs = Type::Arrow(Box::new(lhs), Box::new(rhs));
            if let TokenKind::TyArrow = kind(ps) {
                bump(ps, ext);
            } else {
                break;
            }
        }
    }
    Ok(lhs)
}

pub fn tyabs<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    let tyvar = uppercase_id(ps, ext)?;
    let sp = ps.span;
    let ty: Box<Type<TExtDialect>> = Box::new(Type::Var(ps.tyvar.push(tyvar)));
    let body = once(ps, |p| parse(p, ext), "abstraction body required")?;
    Ok(Term::new(Kind::TyAbs(Box::new(body)), sp + ps.span))
}

pub fn tmabs<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    let tmvar = lowercase_id(ps, ext)?;
    let sp = ps.span;
    ps.tmvar.push(tmvar);

    expect(ps, ext, TokenKind::Colon)?;
    let ty = once(ps, |p| ty(p, ext), "type annotation required in abstraction")?;
    expect(ps, ext, TokenKind::Proj)?;
    let body = once(ps, |p| parse(p, ext), "abstraction body required")?;
    ps.tmvar.pop();
    Ok(Term::new(Kind::Abs(Box::new(ty), Box::new(body)), sp + ps.span))
}

pub fn fold<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    expect(ps, ext, TokenKind::Fold)?;
    let sp = ps.span;
    let ty = once(ps, |p| ty(p, ext), "type annotation required after `fold`")?;
    let tm = once(ps, |p| parse(p, ext), "term required after `fold`")?;
    Ok(Term::new(Kind::Fold(Box::new(ty), Box::new(tm)), sp + ps.span))
}

pub fn unfold<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    expect(ps, ext, TokenKind::Unfold)?;
    let sp = ps.span;
    let ty = once(ps, |p| ty(p, ext), "type annotation required after `unfold`")?;
    let tm = once(ps, |p| parse(p, ext), "term required after `unfold`")?;
    Ok(Term::new(Kind::Unfold(Box::new(ty), Box::new(tm)), sp + ps.span))
}

pub fn fix<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    let sp = ps.span;
    expect(ps, ext, TokenKind::Fix)?;
    let t = parse(ps, ext)?;
    Ok(Term::new(Kind::Fix(Box::new(t)), sp + ps.span))
}

pub fn letexpr<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    let sp = ps.span;
    expect(ps, ext, TokenKind::Let)?;
    let pat = once(ps, |p| pattern(p, ext), "missing pattern")?;

    expect(ps, ext, TokenKind::Equals)?;

    let t1 = once(ps, |p| parse(p, ext), "let binder required")?;
    let len = ps.tmvar.len();
    for var in PatVarStack::<TExtDialect>::collect(&pat, ext, &ps.ext_state)
        .into_iter()
        .rev()
    {
        ps.tmvar.push(var);
    }
    expect(ps, ext, TokenKind::In)?;
    let t2 = once(ps, |p| parse(p, ext), "let body required")?;
    while ps.tmvar.len() > len {
        ps.tmvar.pop();
    }
    Ok(Term::new(
        Kind::Let(Box::new(pat), Box::new(t1), Box::new(t2)),
        sp + ps.span,
    ))
}

pub fn lambda<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    expect(ps, ext, TokenKind::Lambda)?;
    match kind(ps) {
        TokenKind::Uppercase(_) => tyabs(ps, ext),
        TokenKind::Lowercase(_) => tmabs(ps, ext),
        _ => {
            ps.diagnostic
                .push("expected identifier after lambda, found".to_string(), ps.span);
            error(ps, ErrorKind::ExpectedIdent)
        }
    }
}

pub fn paren<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    expect(ps, ext, TokenKind::LParen)?;
    let span = ps.span;

    let mut ext2 = *ext;
    let mut ext3 = *ext;
    let mut n = once_or_more(ps, |p| parse(p, &mut ext2), TokenKind::Comma, &mut ext3)?;
    expect(ps, ext, TokenKind::RParen)?;
    if n.len() > 1 {
        Ok(Term::new(Kind::Product(n), span + ps.span))
    } else {
        // invariant, n.len() >= 1
        Ok(n.remove(0))
    }
}

pub fn uppercase_id<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<String>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    match bump(ps, ext) {
        TokenKind::Uppercase(s) => Ok(s),
        tk => {
            ps.diagnostic
                .push(format!("expected uppercase identifier, found {:?}", tk), ps.span);
            error(ps, ErrorKind::ExpectedIdent)
        }
    }
}

pub fn lowercase_id<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<String>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    match bump(ps, ext) {
        TokenKind::Lowercase(s) => Ok(s),
        tk => {
            ps.diagnostic
                .push(format!("expected lowercase identifier, found {:?}", tk), ps.span);
            error(ps, ErrorKind::ExpectedIdent)
        }
    }
}

pub fn literal<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    let lit = match bump(ps, ext) {
        TokenKind::Nat(x) => Literal::Nat(x),
        TokenKind::True => Literal::Bool(true),
        TokenKind::False => Literal::Bool(false),
        TokenKind::Unit => Literal::Unit,
        TokenKind::Tag(s) => Literal::Tag(s),
        _ => return error(ps, ErrorKind::Unknown),
    };
    Ok(Term::new(Kind::Lit(lit), ps.span))
}

pub fn primitive<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    let p = match bump(ps, ext) {
        TokenKind::IsZero => Primitive::IsZero,
        TokenKind::Succ => Primitive::Succ,
        TokenKind::Pred => Primitive::Pred,
        _ => return error(ps, ErrorKind::Unknown),
    };
    Ok(Term::new(Kind::Primitive(p), ps.span))
}

/// Important to note that this function can push variable names to the
/// de Bruijn naming context. Callers of this function are responsible for
/// making sure that the stack is balanced afterwards
pub fn pat_atom<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Pattern<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    match kind(ps) {
        TokenKind::LParen => pattern(ps, ext),
        TokenKind::Wildcard => {
            bump(ps, ext);
            Ok(Pattern::Any)
        }
        TokenKind::Uppercase(_) => {
            let tycon = uppercase_id(ps, ext)?;
            let inner = match pattern(ps, ext) {
                Ok(pat) => pat,
                _ => Pattern::Any,
            };
            Ok(Pattern::Constructor(tycon, Box::new(inner)))
        }
        TokenKind::Lowercase(_) => {
            let var = lowercase_id(ps, ext)?;
            // ps.tmvar.push(var.clone());
            Ok(Pattern::Variable(var))
        }
        TokenKind::True => {
            bump(ps, ext);
            Ok(Pattern::Literal(Literal::Bool(true)))
        }
        TokenKind::False => {
            bump(ps, ext);
            Ok(Pattern::Literal(Literal::Bool(false)))
        }
        TokenKind::Unit => {
            bump(ps, ext);
            Ok(Pattern::Literal(Literal::Unit))
        }
        TokenKind::Nat(n) => {
            // O great borrowck, may this humble offering appease thee
            let n = *n;
            bump(ps, ext);
            Ok(Pattern::Literal(Literal::Nat(n)))
        }
        TokenKind::Tag(s) => {
            let s = s.clone();
            bump(ps, ext);
            Ok(Pattern::Literal(Literal::Tag(s)))
        }
        _ => error(ps, ErrorKind::ExpectedPattern),
    }
}

pub fn pattern<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Pattern<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    match kind(ps) {
        TokenKind::LParen => {
            bump(ps, ext);
            let mut ext2 = *ext;
            let mut ext3 = *ext;
            let mut v = once_or_more(ps, |p| pat_atom(p, &mut ext2), TokenKind::Comma, &mut ext3)?;
            expect(ps, ext, TokenKind::RParen)?;
            if v.len() > 1 {
                Ok(Pattern::Product(v))
            } else {
                // v must have length == 1, else we would have early returned
                assert_eq!(v.len(), 1);
                Ok(v.remove(0))
            }
        }
        _ => pat_atom(ps, ext),
    }
}

pub fn case_arm<TExtDialect: SystemRDialect, TExt: fmt::Debug + SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Arm<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    // match kind(ps) {
    //     ExtTokenKind::Bar => bump(ps),
    //     _ => return error(ps, ErrorKind::ExpectedToken(ExtTokenKind::Bar)),
    // };

    // We don't track the length of the debruijn index in other methods,
    // but we have a couple branches where variables might be bound,
    // and this is pretty much the easiest way of doing it

    let len = ps.tmvar.len();
    let mut span = ps.span;

    let pat = once(ps, |p| pattern(p, ext), "missing pattern")?;

    for var in PatVarStack::<TExtDialect>::collect(&pat, ext, &ps.ext_state)
        .into_iter()
        .rev()
    {
        ps.tmvar.push(var);
    }

    expect(ps, ext, TokenKind::Equals)?;
    expect(ps, ext, TokenKind::Gt)?;

    let term = Box::new(once(ps, |p| application(p, ext), "missing case term")?);

    bump_if(ps, ext, &TokenKind::Comma);

    // Unbind any variables from the parsing context
    while ps.tmvar.len() > len {
        ps.tmvar.pop();
    }

    span += ps.span;

    Ok(Arm { span, pat, term })
}

pub fn case<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    expect(ps, ext, TokenKind::Case)?;
    let span = ps.span;
    let expr = once(ps, |p| parse(p, ext), "missing case expression")?;
    expect(ps, ext, TokenKind::Of)?;

    bump_if(ps, ext, &TokenKind::Bar);

    let mut ext2 = *ext;
    let mut ext3 = *ext;
    let arms = once_or_more(ps, |p| case_arm(p, &mut ext3), TokenKind::Bar, &mut ext2)?;

    Ok(Term::new(Kind::Case(Box::new(expr), arms), span + ps.span))
}

pub fn injection<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    let label = uppercase_id(ps, ext)?;
    let sp = ps.span;
    let term = match parse(ps, ext) {
        Ok(t) => t,
        _ => Term::new(Kind::Lit(Literal::Unit), ps.span),
    };

    expect(ps, ext, TokenKind::Of)?;
    let ty = ty(ps, ext)?;
    Ok(Term::new(
        Kind::Injection(label, Box::new(term), Box::new(ty)),
        sp + ps.span,
    ))
}

pub fn pack<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    expect(ps, ext, TokenKind::Pack)?;
    let sp = ps.span;
    let witness = ty(ps, ext)?;
    expect(ps, ext, TokenKind::Comma)?;
    let evidence = parse(ps, ext)?;
    expect(ps, ext, TokenKind::As)?;
    let signature = ty(ps, ext)?;

    Ok(Term::new(
        Kind::Pack(Box::new(witness), Box::new(evidence), Box::new(signature)),
        sp + ps.span,
    ))
}

pub fn unpack<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    expect(ps, ext, TokenKind::Unpack)?;
    let sp = ps.span;
    let package = parse(ps, ext)?;
    expect(ps, ext, TokenKind::As)?;

    let tyvar = uppercase_id(ps, ext)?;
    expect(ps, ext, TokenKind::Comma)?;
    let name = lowercase_id(ps, ext)?;
    ps.tyvar.push(tyvar);
    ps.tmvar.push(name);
    expect(ps, ext, TokenKind::In)?;
    let expr = parse(ps, ext)?;
    ps.tmvar.pop();
    ps.tyvar.pop();
    Ok(Term::new(Kind::Unpack(Box::new(package), Box::new(expr)), sp + ps.span))
}

pub fn numeric_bytes_arr<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    // advance past opening LSquare
    let start_span = ps.span;
    let start_token = ps.token.clone();
    bump(ps, ext);

    let mut ext_2 = *ext;
    let nats = once_or_more(ps, |p| atom(p, &mut ext_2), TokenKind::Comma, ext)?;
    let mut out_bytes: Vec<u8> = Vec::new();
    for n in nats {
        match n.kind {
            Kind::Lit(Literal::Nat(n)) => match n {
                n @ 0..256 => {
                    out_bytes.push(n as u8);
                }
                e => {
                    return Err(Error {
                        span: start_span,
                        tok: start_token,
                        kind: ErrorKind::ExtendedError(format!("expect Nat literals from 0 to 255, got {:?}", e)),
                    }
                    .into());
                }
            },
            v => {
                return Err(Error {
                    span: start_span,
                    tok: start_token,
                    kind: ErrorKind::ExtendedError(format!("expect Nat literals from 0 to 255, got {:?}", v)),
                }
                .into())
            }
        }
    }

    // lastly we should expect a closing RSquare, no trailing comma!
    expect(ps, ext, TokenKind::RSquare)?;

    let mut bytes = Term::unit();
    bytes.span = Span {
        start: start_span.start,
        end: ps.span.end,
    };
    bytes.kind = Kind::Lit(Literal::Bytes(out_bytes));
    Ok(bytes)
}

pub fn atom<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    match kind(ps) {
        TokenKind::Extended(tk) => ext_atom(ps, ext),
        TokenKind::LSquare => numeric_bytes_arr(ps, ext),
        TokenKind::LParen => paren(ps, ext),
        TokenKind::Fix => fix(ps, ext),
        TokenKind::Fold => fold(ps, ext),
        TokenKind::Unfold => unfold(ps, ext),
        TokenKind::Pack => pack(ps, ext),
        TokenKind::Unpack => unpack(ps, ext),
        TokenKind::IsZero | TokenKind::Succ | TokenKind::Pred => primitive(ps, ext),
        TokenKind::Uppercase(_) => injection(ps, ext),
        TokenKind::Lowercase(s) => {
            let var = lowercase_id(ps, ext)?;
            match ps.tmvar.lookup(&var) {
                Some(idx) => Ok(Term::new(Kind::Var(idx), ps.span)),
                None => {
                    if let Some(idx) = ps.platform_bindings.has(&var) {
                        return Ok(Term::new(Kind::PlatformBinding(idx), ps.span));
                    }
                    ps.diagnostic.push(format!("parser: unbound variable {}", var), ps.span);
                    error(ps, ErrorKind::UnboundTypeVar)
                }
            }
        }
        TokenKind::Nat(_) | TokenKind::True | TokenKind::False | TokenKind::Unit | TokenKind::Tag(_) => {
            literal(ps, ext)
        }
        TokenKind::Eof => error(ps, ErrorKind::Eof),
        TokenKind::Semicolon => {
            bump(ps, ext);
            error(ps, ErrorKind::ExpectedAtom)
        }
        _ => error(ps, ErrorKind::ExpectedAtom),
    }
}

/// Parse a term of form:
/// projection = atom `.` projection
/// projection = atom
pub fn projection<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    let atom = atom(ps, ext)?;
    if bump_if(ps, ext, &TokenKind::Proj) {
        let idx = match bump(ps, ext) {
            TokenKind::Nat(idx) => idx,
            _ => {
                ps.diagnostic
                    .push(format!("expected integer index after {}", atom), ps.span);
                return error(ps, ErrorKind::ExpectedToken(TokenKind::Proj));
            }
        };
        let sp = atom.span + ps.span;
        Ok(Term::new(Kind::Projection(Box::new(atom), idx as usize), sp))
    } else {
        Ok(atom)
    }
}

/// Parse an application of form:
/// application = atom application' | atom
/// application' = atom application' | empty
pub fn application<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    let mut app = projection(ps, ext)?;

    loop {
        let sp = app.span;
        if let Ok(ty) = ty_app(ps, ext) {
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
            app = Term::new(Kind::TyApp(Box::new(app), Box::new(ty)), sp + ps.span);
        } else if let Ok(term) = projection(ps, ext) {
            app = Term::new(Kind::App(Box::new(app), Box::new(term)), sp + ps.span);
        } else {
            break;
        }
    }
    Ok(app)
}

pub fn ext_atom<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    let r = {
        match ext.parser_ext_atom(ps) {
            Err(e) => return Err(e),
            Ok(t) => t,
        }
    };
    Ok(r)
}

pub fn ext_parse<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    let r = {
        match ext.parser_ext_parse(ps) {
            Err(e) => return Err(e),
            Ok(t) => t,
        }
    };
    Ok(r)
}

pub fn parse<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>>
where
    <TExtDialect as SystemRDialect>::TokenKind: 'static,
{
    match kind(ps).clone() {
        TokenKind::Case => case(ps, ext),
        TokenKind::Lambda => lambda(ps, ext),
        TokenKind::Let => letexpr(ps, ext),
        TokenKind::Extended(tk) if ext.parser_has_ext_parse(&tk) => ext_parse(ps, ext),
        _ => application(ps, ext),
    }
}
