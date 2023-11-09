use super::debruijn::DeBruijnIndexer;
use super::error::Error;
use super::lexer::ExtLexer;
use super::{ExtTokenKind, Token};
use crate::bottom::{BottomDialect, BottomExtension};
use crate::dialect::{SystemRDialect, SystemRExtension};

use crate::system_r_util::diagnostic::Diagnostic;
use crate::system_r_util::span::*;
use core::fmt;
use std::hash;

use crate::patterns::{PatVarStack, Pattern};
use crate::platform_bindings::PlatformBindings;
use crate::terms::*;
use crate::types::*;

#[derive(Default, Debug, Clone)]
pub struct ParserState<
    's,
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
> {
    pub tmvar: DeBruijnIndexer,
    pub tyvar: DeBruijnIndexer,
    pub diagnostic: Diagnostic<'s>,
    pub lexer: ExtLexer<'s, TExtDialect>,
    pub span: Span,
    pub token: Token<TExtDialect::TExtTokenKind>,
    pub platform_bindings: PlatformBindings,
    pub ext_state: TExtDialect::TExtDialectState,
    pub ty: TExtDialect::TExtType,
}

#[derive(Clone, Debug)]
pub enum ErrorKind<TExtTokenKind: PartialEq> {
    ExpectedAtom,
    ExpectedIdent,
    ExpectedType,
    ExpectedPattern,
    ExpectedToken(ExtTokenKind<TExtTokenKind>),
    UnboundTypeVar,
    Unknown,
    Eof,
    ExtendedError(String),
}

impl<'s, TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd>
    ParserState<'s, TExtDialect>
{
    pub fn die(self) -> String {
        let d = self.diagnostic;
        d.emit()
    }

    pub fn to_ext_state(self) -> TExtDialect::TExtDialectState {
        self.ext_state
    }
}

pub fn new<'s>(platform_bindings: &'s PlatformBindings, input: &'s str) -> ParserState<'s, BottomDialect> {
    let mut ext = BottomExtension;
    ext_new(platform_bindings, input, &mut ext)
}

pub fn ext_new<
    's,
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    platform_bindings: &'s PlatformBindings,
    input: &'s str,
    ext: &mut TExt,
) -> ParserState<'s, TExtDialect> {
    let mut p = ParserState {
        tmvar: DeBruijnIndexer::default(),
        tyvar: DeBruijnIndexer::default(),
        diagnostic: Diagnostic::new(input),
        lexer: ExtLexer::new(input.chars()),
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
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    F: FnMut(&mut ParserState<TExtDialect>) -> Result<T, Error<TExtDialect::TExtTokenKind>>,
    TExt: Clone + Copy + Default + SystemRExtension<TExtDialect>,
    T,
>(
    ps: &mut ParserState<TExtDialect>,
    mut func: F,
    delimiter: ExtTokenKind<TExtDialect::TExtTokenKind>,
    ext: &mut TExt,
) -> Result<Vec<T>, Error<TExtDialect::TExtTokenKind>> {
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
pub fn once<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    F: FnMut(&mut ParserState<TExtDialect>) -> Result<T, Error<TExtDialect::TExtTokenKind>>,
    T,
>(
    ps: &mut ParserState<TExtDialect>,
    mut func: F,
    message: &str,
) -> Result<T, Error<TExtDialect::TExtTokenKind>> {
    match func(ps) {
        Err(e) => {
            ps.diagnostic.push(message, ps.span);
            Err(e)
        }
        t => t,
    }
}

pub fn diagnostic<
    's,
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
>(
    ps: ParserState<'s, TExtDialect>,
) -> Diagnostic<'s> {
    ps.diagnostic
}

pub fn error<
    's,
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TError,
>(
    ps: &ParserState<'s, TExtDialect>,
    kind: ErrorKind<TExtDialect::TExtTokenKind>,
) -> Result<TError, Error<TExtDialect::TExtTokenKind>> {
    Err(Error {
        span: ps.token.span,
        tok: ps.token.clone(),
        kind,
    })
}

pub fn bump<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> ExtTokenKind<TExtDialect::TExtTokenKind> {
    let prev = std::mem::replace(&mut ps.token, ps.lexer.lex(ext));
    ps.span = prev.span;
    prev.kind
}

/// Used within parser-logic to advance the ParserState to the next token,
/// if the current token's kind matches the given `kind` arg and returning
/// true, otherwise returns false
pub fn bump_if<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
    kind: &ExtTokenKind<TExtDialect::TExtTokenKind>,
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
pub fn expect<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
    kind: ExtTokenKind<TExtDialect::TExtTokenKind>,
) -> Result<(), Error<TExtDialect::TExtTokenKind>> {
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
pub fn kind<
    's,
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
>(
    ps: &'s ParserState<TExtDialect>,
) -> &'s ExtTokenKind<TExtDialect::TExtTokenKind> {
    &ps.token.kind
}

pub fn ty_variant<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Variant<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    let label = uppercase_id(ps, ext)?;
    let ty = match ty(ps, ext) {
        Ok(ty) => ty,
        _ => Type::Unit,
    };

    Ok(Variant { label, ty })
}

pub fn ty_app<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Type<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    if !bump_if(ps, ext, &ExtTokenKind::LSquare) {
        return error(ps, ErrorKind::ExpectedToken(ExtTokenKind::LSquare));
    }
    let ty = ty(ps, ext)?;
    expect(ps, ext, ExtTokenKind::RSquare)?;
    Ok(ty)
}

pub fn ty_atom<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Type<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    match kind(ps) {
        ExtTokenKind::Tag(s) => {
            let v = s.to_owned();
            bump(ps, ext);
            Ok(Type::Tag(v))
        }
        ExtTokenKind::TyBool => {
            bump(ps, ext);
            Ok(Type::Bool)
        }
        ExtTokenKind::TyNat => {
            bump(ps, ext);
            Ok(Type::Nat)
        }
        ExtTokenKind::TyUnit => {
            bump(ps, ext);
            Ok(Type::Unit)
        }
        ExtTokenKind::LParen => {
            bump(ps, ext);
            let r = ty(ps, ext)?;
            expect(ps, ext, ExtTokenKind::RParen)?;
            Ok(r)
        }
        ExtTokenKind::Forall => {
            bump(ps, ext);
            Ok(Type::Universal(Box::new(ty(ps, ext)?)))
        }
        ExtTokenKind::Exists => {
            bump(ps, ext);
            let tvar = uppercase_id(ps, ext)?;
            expect(ps, ext, ExtTokenKind::Proj)?;
            ps.tyvar.push(tvar);
            let xs = Type::Existential(Box::new(ty(ps, ext)?));
            ps.tyvar.pop();
            Ok(xs)
        }
        ExtTokenKind::Uppercase(_) => {
            let ty = uppercase_id(ps, ext)?;
            match ps.tyvar.lookup(&ty) {
                Some(idx) => Ok(Type::Var(idx)),
                None => Ok(Type::Alias(ty)),
            }
        }
        ExtTokenKind::LBrace => {
            bump(ps, ext);
            let mut ext2 = ext.clone();
            let mut ext3 = ext.clone();
            let fields = once_or_more(ps, |p| ty_variant(p, &mut ext2), ExtTokenKind::Bar, &mut ext3)?;
            expect(ps, ext, ExtTokenKind::RBrace)?;
            Ok(Type::Variant(fields))
        }
        v => error(ps, ErrorKind::ExtendedError(format!("Expected type-able token, got {:?}", v))),
    }
}

pub fn ty_tuple<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Type<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    if bump_if(ps, ext, &ExtTokenKind::LParen) {
        let mut ext2 = ext.clone();
        let mut ext3 = ext.clone();
        let mut v = once_or_more(ps, |p| ty(p, &mut ext2), ExtTokenKind::Comma, &mut ext3)?;
        expect(ps, ext, ExtTokenKind::RParen)?;

        if v.len() > 1 {
            Ok(Type::Product(v))
        } else {
            Ok(v.remove(0))
        }
    } else {
        ty_atom(ps, ext)
    }
}

pub fn ty<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Type<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    /*
    match kind(ps) {
        ExtTokenKind::AnyTag => return Ok(Type::AnyTag),
        ExtTokenKind::Tag(s) => return Ok(Type::Tag(s.to_owned())),
        _ => {}
    };
    */
    if ext.parser_ty_bump_if(ps) {
        return ext.parser_ty(ps);
    }

    if bump_if(ps, ext, &ExtTokenKind::Rec) {
        let name = uppercase_id(ps, ext)?;
        expect(ps, ext, ExtTokenKind::Equals)?;
        ps.tyvar.push(name);
        let ty = ty(ps, ext)?;
        ps.tyvar.pop();
        return Ok(Type::Rec(Box::new(ty)));
    }

    let mut lhs = ty_tuple(ps, ext)?;
    if let ExtTokenKind::TyArrow = kind(ps) {
        bump(ps, ext);
        while let Ok(rhs) = ty(ps, ext) {
            lhs = Type::Arrow(Box::new(lhs), Box::new(rhs));
            if let ExtTokenKind::TyArrow = kind(ps) {
                bump(ps, ext);
            } else {
                break;
            }
        }
    }
    Ok(lhs)
}

pub fn tyabs<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: Default + fmt::Debug + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    let tyvar = uppercase_id(ps, ext)?;
    let sp = ps.span;
    let ty: Box<Type<TExtDialect>> = Box::new(Type::Var(ps.tyvar.push(tyvar)));
    let body = once(ps, |p| parse(p, ext), "abstraction body required")?;
    Ok(Term::new(Kind::TyAbs(Box::new(body)), sp + ps.span))
}

pub fn tmabs<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: Default + fmt::Debug + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    let tmvar = lowercase_id(ps, ext)?;
    let sp = ps.span;
    ps.tmvar.push(tmvar);

    expect(ps, ext, ExtTokenKind::Colon)?;
    let ty = once(ps, |p| ty(p, ext), "type annotation required in abstraction")?;
    expect(ps, ext, ExtTokenKind::Proj)?;
    let body = once(ps, |p| parse(p, ext), "abstraction body required")?;
    ps.tmvar.pop();
    Ok(Term::new(Kind::Abs(Box::new(ty), Box::new(body)), sp + ps.span))
}

pub fn fold<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: Default + fmt::Debug + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    expect(ps, ext, ExtTokenKind::Fold)?;
    let sp = ps.span;
    let ty = once(ps, |p| ty(p, ext), "type annotation required after `fold`")?;
    let tm = once(ps, |p| parse(p, ext), "term required after `fold`")?;
    Ok(Term::new(Kind::Fold(Box::new(ty), Box::new(tm)), sp + ps.span))
}

pub fn unfold<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    expect(ps, ext, ExtTokenKind::Unfold)?;
    let sp = ps.span;
    let ty = once(ps, |p| ty(p, ext), "type annotation required after `unfold`")?;
    let tm = once(ps, |p| parse(p, ext), "term required after `unfold`")?;
    Ok(Term::new(Kind::Unfold(Box::new(ty), Box::new(tm)), sp + ps.span))
}

pub fn fix<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    let sp = ps.span;
    expect(ps, ext, ExtTokenKind::Fix)?;
    let t = parse(ps, ext)?;
    Ok(Term::new(Kind::Fix(Box::new(t)), sp + ps.span))
}

pub fn letexpr<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    let sp = ps.span;
    expect(ps, ext, ExtTokenKind::Let)?;
    let pat = once(ps, |p| pattern(p, ext), "missing pattern")?;

    expect(ps, ext, ExtTokenKind::Equals)?;

    let t1 = once(ps, |p| parse(p, ext), "let binder required")?;
    let len = ps.tmvar.len();
    for var in PatVarStack::<TExtDialect>::collect(&pat, ext, &mut ps.ext_state)
        .into_iter()
        .rev()
    {
        ps.tmvar.push(var);
    }
    expect(ps, ext, ExtTokenKind::In)?;
    let t2 = once(ps, |p| parse(p, ext), "let body required")?;
    while ps.tmvar.len() > len {
        ps.tmvar.pop();
    }
    Ok(Term::new(
        Kind::Let(Box::new(pat), Box::new(t1), Box::new(t2)),
        sp + ps.span,
    ))
}

pub fn lambda<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    expect(ps, ext, ExtTokenKind::Lambda)?;
    match kind(ps) {
        ExtTokenKind::Uppercase(_) => tyabs(ps, ext),
        ExtTokenKind::Lowercase(_) => tmabs(ps, ext),
        _ => {
            ps.diagnostic
                .push("expected identifier after lambda, found".to_string(), ps.span);
            error(ps, ErrorKind::ExpectedIdent)
        }
    }
}

pub fn paren<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    expect(ps, ext, ExtTokenKind::LParen)?;
    let span = ps.span;

    let mut ext2 = ext.clone();
    let mut ext3 = ext.clone();
    let mut n = once_or_more(ps, |p| parse(p, &mut ext2), ExtTokenKind::Comma, &mut ext3)?;
    expect(ps, ext, ExtTokenKind::RParen)?;
    if n.len() > 1 {
        Ok(Term::new(Kind::Product(n), span + ps.span))
    } else {
        // invariant, n.len() >= 1
        Ok(n.remove(0))
    }
}

pub fn uppercase_id<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<String, Error<TExtDialect::TExtTokenKind>> {
    match bump(ps, ext) {
        ExtTokenKind::Uppercase(s) => Ok(s),
        tk => {
            ps.diagnostic
                .push(format!("expected uppercase identifier, found {:?}", tk), ps.span);
            error(ps, ErrorKind::ExpectedIdent)
        }
    }
}

pub fn lowercase_id<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<String, Error<TExtDialect::TExtTokenKind>> {
    match bump(ps, ext) {
        ExtTokenKind::Lowercase(s) => Ok(s),
        tk => {
            ps.diagnostic
                .push(format!("expected lowercase identifier, found {:?}", tk), ps.span);
            error(ps, ErrorKind::ExpectedIdent)
        }
    }
}

pub fn literal<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    let lit = match bump(ps, ext) {
        ExtTokenKind::Nat(x) => Literal::Nat(x),
        ExtTokenKind::True => Literal::Bool(true),
        ExtTokenKind::False => Literal::Bool(false),
        ExtTokenKind::Unit => Literal::Unit,
        ExtTokenKind::Tag(s) => Literal::Tag(s),
        _ => return error(ps, ErrorKind::Unknown),
    };
    Ok(Term::new(Kind::Lit(lit), ps.span))
}

pub fn primitive<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    let p = match bump(ps, ext) {
        ExtTokenKind::IsZero => Primitive::IsZero,
        ExtTokenKind::Succ => Primitive::Succ,
        ExtTokenKind::Pred => Primitive::Pred,
        _ => return error(ps, ErrorKind::Unknown),
    };
    Ok(Term::new(Kind::Primitive(p), ps.span))
}

/// Important to note that this function can push variable names to the
/// de Bruijn naming context. Callers of this function are responsible for
/// making sure that the stack is balanced afterwards
pub fn pat_atom<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Pattern<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    match kind(ps) {
        ExtTokenKind::LParen => pattern(ps, ext),
        ExtTokenKind::Wildcard => {
            bump(ps, ext);
            Ok(Pattern::Any)
        }
        ExtTokenKind::Uppercase(_) => {
            let tycon = uppercase_id(ps, ext)?;
            let inner = match pattern(ps, ext) {
                Ok(pat) => pat,
                _ => Pattern::Any,
            };
            Ok(Pattern::Constructor(tycon, Box::new(inner)))
        }
        ExtTokenKind::Lowercase(_) => {
            let var = lowercase_id(ps, ext)?;
            // ps.tmvar.push(var.clone());
            Ok(Pattern::Variable(var))
        }
        ExtTokenKind::True => {
            bump(ps, ext);
            Ok(Pattern::Literal(Literal::Bool(true)))
        }
        ExtTokenKind::False => {
            bump(ps, ext);
            Ok(Pattern::Literal(Literal::Bool(false)))
        }
        ExtTokenKind::Unit => {
            bump(ps, ext);
            Ok(Pattern::Literal(Literal::Unit))
        }
        ExtTokenKind::Nat(n) => {
            // O great borrowck, may this humble offering appease thee
            let n = *n;
            bump(ps, ext);
            Ok(Pattern::Literal(Literal::Nat(n)))
        }
        ExtTokenKind::Tag(s) => {
            let s = s.clone();
            bump(ps, ext);
            Ok(Pattern::Literal(Literal::Tag(s)))
        }
        _ => error(ps, ErrorKind::ExpectedPattern),
    }
}

pub fn pattern<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Pattern<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    match kind(ps) {
        ExtTokenKind::LParen => {
            bump(ps, ext);
            let mut ext2 = ext.clone();
            let mut ext3 = ext.clone();
            let mut v = once_or_more(ps, |p| pat_atom(p, &mut ext2), ExtTokenKind::Comma, &mut ext3)?;
            expect(ps, ext, ExtTokenKind::RParen)?;
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

pub fn case_arm<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + fmt::Debug + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Arm<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
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

    for var in PatVarStack::<TExtDialect>::collect(&pat, ext, &mut ps.ext_state)
        .into_iter()
        .rev()
    {
        ps.tmvar.push(var);
    }

    expect(ps, ext, ExtTokenKind::Equals)?;
    expect(ps, ext, ExtTokenKind::Gt)?;

    let term = Box::new(once(ps, |p| application(p, ext), "missing case term")?);

    bump_if(ps, ext, &ExtTokenKind::Comma);

    // Unbind any variables from the parsing context
    while ps.tmvar.len() > len {
        ps.tmvar.pop();
    }

    span += ps.span;

    Ok(Arm { span, pat, term })
}

pub fn case<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + fmt::Debug + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    expect(ps, ext, ExtTokenKind::Case)?;
    let span = ps.span;
    let expr = once(ps, |p| parse(p, ext), "missing case expression")?;
    expect(ps, ext, ExtTokenKind::Of)?;

    bump_if(ps, ext, &ExtTokenKind::Bar);

    let mut ext2 = ext.clone();
    let mut ext3 = ext.clone();
    let arms = once_or_more(ps, |p| case_arm(p, &mut ext3), ExtTokenKind::Bar, &mut ext2)?;

    Ok(Term::new(Kind::Case(Box::new(expr), arms), span + ps.span))
}

pub fn injection<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    let label = uppercase_id(ps, ext)?;
    let sp = ps.span;
    let term = match parse(ps, ext) {
        Ok(t) => t,
        _ => Term::new(Kind::Lit(Literal::Unit), ps.span),
    };

    expect(ps, ext, ExtTokenKind::Of)?;
    let ty = ty(ps, ext)?;
    Ok(Term::new(
        Kind::Injection(label, Box::new(term), Box::new(ty)),
        sp + ps.span,
    ))
}

pub fn pack<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    expect(ps, ext, ExtTokenKind::Pack)?;
    let sp = ps.span;
    let witness = ty(ps, ext)?;
    expect(ps, ext, ExtTokenKind::Comma)?;
    let evidence = parse(ps, ext)?;
    expect(ps, ext, ExtTokenKind::As)?;
    let signature = ty(ps, ext)?;

    Ok(Term::new(
        Kind::Pack(Box::new(witness), Box::new(evidence), Box::new(signature)),
        sp + ps.span,
    ))
}

pub fn unpack<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    expect(ps, ext, ExtTokenKind::Unpack)?;
    let sp = ps.span;
    let package = parse(ps, ext)?;
    expect(ps, ext, ExtTokenKind::As)?;

    let tyvar = uppercase_id(ps, ext)?;
    expect(ps, ext, ExtTokenKind::Comma)?;
    let name = lowercase_id(ps, ext)?;
    ps.tyvar.push(tyvar);
    ps.tmvar.push(name);
    expect(ps, ext, ExtTokenKind::In)?;
    let expr = parse(ps, ext)?;
    ps.tmvar.pop();
    ps.tyvar.pop();
    Ok(Term::new(Kind::Unpack(Box::new(package), Box::new(expr)), sp + ps.span))
}

pub fn atom<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    match kind(ps) {
        ExtTokenKind::Extended(tk) => ext_atom(ps, ext),
        ExtTokenKind::LParen => paren(ps, ext),
        ExtTokenKind::Fix => fix(ps, ext),
        ExtTokenKind::Fold => fold(ps, ext),
        ExtTokenKind::Unfold => unfold(ps, ext),
        ExtTokenKind::Pack => pack(ps, ext),
        ExtTokenKind::Unpack => unpack(ps, ext),
        ExtTokenKind::IsZero | ExtTokenKind::Succ | ExtTokenKind::Pred => primitive(ps, ext),
        ExtTokenKind::Uppercase(_) => injection(ps, ext),
        ExtTokenKind::Lowercase(s) => {
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
        ExtTokenKind::Nat(_) | ExtTokenKind::True | ExtTokenKind::False | ExtTokenKind::Unit | ExtTokenKind::Tag(_) => {
            literal(ps, ext)
        }
        ExtTokenKind::Eof => error(ps, ErrorKind::Eof),
        ExtTokenKind::Semicolon => {
            bump(ps, ext);
            error(ps, ErrorKind::ExpectedAtom)
        }
        _ => error(ps, ErrorKind::ExpectedAtom),
    }
}

/// Parse a term of form:
/// projection = atom `.` projection
/// projection = atom
pub fn projection<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    let atom = atom(ps, ext)?;
    if bump_if(ps, ext, &ExtTokenKind::Proj) {
        let idx = match bump(ps, ext) {
            ExtTokenKind::Nat(idx) => idx,
            _ => {
                ps.diagnostic
                    .push(format!("expected integer index after {}", atom), ps.span);
                return error(ps, ErrorKind::ExpectedToken(ExtTokenKind::Proj));
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
pub fn application<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
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

pub fn ext_atom<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    let r = {
        match ext.parser_ext_atom(ps) {
            Err(e) => return Err(e),
            Ok(t) => t,
        }
    };
    return Ok(r);
}

pub fn ext_parse<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    let r = {
        match ext.parser_ext_parse(ps) {
            Err(e) => return Err(e),
            Ok(t) => t,
        }
    };
    return Ok(r);
}

pub fn parse<
    TExtDialect: hash::Hash + Eq + SystemRDialect + Clone + fmt::Debug + Default + PartialEq + PartialOrd,
    TExt: fmt::Debug + Default + Copy + Clone + SystemRExtension<TExtDialect>,
>(
    ps: &mut ParserState<TExtDialect>,
    ext: &mut TExt,
) -> Result<Term<TExtDialect>, Error<TExtDialect::TExtTokenKind>> {
    match kind(ps).clone() {
        ExtTokenKind::Case => case(ps, ext),
        ExtTokenKind::Lambda => lambda(ps, ext),
        ExtTokenKind::Let => letexpr(ps, ext),
        ExtTokenKind::Extended(tk) if ext.parser_has_ext_parse(&tk) => ext_parse(ps, ext),
        _ => application(ps, ext),
    }
}
