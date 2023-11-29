use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    bottom::{BottomDialect, BottomKind, BottomPattern, BottomTokenKind},
    diagnostics::Diagnostic,
    patterns::Pattern,
    platform_bindings::PlatformBindings,
    syntax::{
        error::Error,
        parser,
        parser::{ErrorKind, ParserState},
        TokenKind,
    },
    terms::{Kind, Term},
    type_check::{patterns::overlap, visit::Subst, Type, TypeChecker},
    visit::{
        DialectChangingPatternVisitor, DialectChangingTermVisitor, DialectChangingTypeVisitor, MutTypeVisitor,
        PatternVisitor,
    },
};

use super::{
    ExtendedDialectState, ExtendedKind, ExtendedPattern, ExtendedTokenKind, ExtendedType, SystemRDialect,
    SystemRExtension, SystemRResolver,
};

use anyhow::Result;

#[derive(Copy, Clone, Debug, Default)]
pub struct TypeAliasExtension;

pub type TypeAliasContext = TypeChecker<TypeAliasDialect>;

pub fn new<'s>(platform_bindings: &'s PlatformBindings, input: &'s str) -> ParserState<'s, TypeAliasDialect> {
    parser::ext_new::<TypeAliasDialect, TypeAliasExtension>(platform_bindings, input, &mut TypeAliasExtension)
}

/// Extension 1: TypeAliasDialect
/// - Extends the Bottom-dialect of system_r (ie system_f with some minor
///   kind/type enhancement)
#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub enum TypeAliasTokenKind {
    #[default]
    Placeholder,
    TypeAliasKeyword,
    TypeBindingVar(String),
    Below(BottomTokenKind),
}

impl Display for TypeAliasTokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypeAliasTokenKind")
            .field("variant", &format!("{:?}", self))
            .finish()
    }
}

impl ExtendedTokenKind for TypeAliasTokenKind {}

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub enum TypeAliasKind {
    #[default]
    Empty,
    Declaration(Box<Type<TypeAliasDialect>>),
    StructIdent(String),
    /// fields: struct identifier with leading $, type shape, struct scope body
    /// (in let-polymorphism style)
    TypeAliasExpr(
        String,
        Box<(usize, Type<TypeAliasDialect>)>,
        Box<Term<TypeAliasDialect>>,
    ),
    Below(BottomKind),
}
impl ExtendedKind for TypeAliasKind {}

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub enum TypeAliasPattern {
    #[default]
    Empty,
    Below(BottomPattern),
}
impl ExtendedPattern for TypeAliasPattern {}

#[derive(Clone, Debug, Default, PartialEq, PartialOrd, Eq, Hash)]
pub enum TypeAliasType {
    #[default]
    Empty,
    TypeAliasApp(String, Vec<Type<TypeAliasDialect>>),
    VarWrap(usize),
}
impl ExtendedType for TypeAliasType {}

#[derive(Clone, Default, Debug)]
pub struct TypeAliasDialectState {
    type_map: HashMap<String, (usize, Type<TypeAliasDialect>)>,
}
impl ExtendedDialectState for TypeAliasDialectState {}

#[derive(Hash, Clone, Debug, Default, PartialEq, PartialOrd, Eq)]
pub struct TypeAliasDialect;

impl SystemRDialect for TypeAliasDialect {
    type DialectState = TypeAliasDialectState;
    type Kind = TypeAliasKind;
    type Pattern = TypeAliasPattern;
    type TokenKind = TypeAliasTokenKind;
    type Type = TypeAliasType;
}

pub const KEYWORD_TYPE: &str = "type";

impl SystemRExtension<TypeAliasDialect> for TypeAliasExtension {
    fn lex_is_ext_single(&self, x: char) -> bool {
        x == '$'
    }

    fn lex_is_extended_single_pred(&self, x: char) -> bool {
        x == '$' || (!x.is_whitespace() && x.is_ascii_alphanumeric())
    }

    fn lex_is_ext_keyword(&self, data: &str) -> bool {
        data == KEYWORD_TYPE
    }

    fn lex_extended_single(&mut self, data: &str) -> TypeAliasTokenKind {
        TypeAliasTokenKind::TypeBindingVar(data.to_owned())
    }

    fn lex_ext_keyword(&mut self, data: &str) -> TypeAliasTokenKind {
        if data == KEYWORD_TYPE {
            return TypeAliasTokenKind::TypeAliasKeyword;
        }
        panic!("called lex_ext_keyword with a data str that wasn't TypeAlias-related; shouldn't happen");
    }

    fn pat_ext_pattern_type_eq(
        &self,
        ctx: &TypeChecker<TypeAliasDialect>,
        pat: &TypeAliasPattern,
        ty: &crate::type_check::Type<TypeAliasDialect>,
    ) -> bool {
        false
    }

    fn pat_add_ext_pattern<'a>(
        &'a self,
        parent: &crate::type_check::patterns::Matrix<'a, TypeAliasDialect>,
        ext_pattern: &crate::patterns::Pattern<TypeAliasDialect>,
    ) -> bool {
        false
    }

    fn pat_ext_matches(&self, pat: &TypeAliasPattern, term: &crate::terms::Term<TypeAliasDialect>) -> bool {
        false
    }

    fn parser_has_ext_parse(&self, tk: &TypeAliasTokenKind) -> bool {
        tk == &TypeAliasTokenKind::TypeAliasKeyword
    }

    fn parser_ext_parse<'s>(&mut self, ps: &mut ParserState<TypeAliasDialect>) -> Result<Term<TypeAliasDialect>> {
        let sp = ps.span;
        parser::expect(ps, self, TokenKind::Extended(TypeAliasTokenKind::TypeAliasKeyword))?;

        let struct_ident = parser::once(ps, |p| parser::atom(p, self), "missing struct identifier $Binding")?;
        let struct_ident = match struct_ident.kind {
            Kind::Extended(TypeAliasKind::StructIdent(s)) => s,
            e => {
                return Err(Error {
                    span: ps.span,
                    tok: ps.token.clone(),
                    kind: ErrorKind::ExtendedError::<TypeAliasTokenKind>(format!(
                        "expected a StructIdent, got {:?}",
                        e
                    )),
                }
                .into())
            }
        };

        parser::expect(ps, self, TokenKind::Equals)?;

        // this should be a "holed-out" type specification (it should have
        // entries for where generic types can be substituted-in)
        let type_shape = parse_holed_type_from_decl(ps, self)?;
        set_holed_type_for(ps, &struct_ident, type_shape.clone())?;

        let len = ps.tmvar.len();
        parser::expect(ps, self, TokenKind::In)?;

        let t2 = parser::once(ps, |p| parser::parse(p, self), "type scope body required")?;
        while ps.tmvar.len() > len {
            ps.tmvar.pop();
        }

        // Optionally: return the whole structure, move erasure
        // logic to resolve()
        //Ok(Term {span: sp, kind:
        // Kind::Extended(TypeAliasKind::TypeAliasExpr(struct_ident,
        // Box::new(type_shape), Box::new(t2)))})
        // but actually erasure is easy and makes sense in this case
        Ok(t2)
    }

    fn parser_has_ext_atom(&self, tk: &TypeAliasTokenKind) -> bool {
        false
    }

    fn parser_ext_atom(&mut self, ps: &mut ParserState<'_, TypeAliasDialect>) -> Result<Term<TypeAliasDialect>> {
        let name_tok = ps.token.clone();
        let name_val = match name_tok.kind.clone() {
            TokenKind::Extended(n) => match n {
                TypeAliasTokenKind::TypeBindingVar(name) => name,
                v => {
                    return Err(Error {
                        span: name_tok.span,
                        tok: name_tok.clone(),
                        kind: ErrorKind::ExtendedError::<TypeAliasTokenKind>(format!(
                            "Expected a TypeBindingVar, got {:?}",
                            v
                        )),
                    }
                    .into())
                }
            },
            v => {
                return Err(Error {
                    span: name_tok.span,
                    tok: name_tok.clone(),
                    kind: ErrorKind::ExtendedError::<TypeAliasTokenKind>(format!(
                        "Expected a ExtendedTokenKind, got {:?}",
                        v
                    )),
                }
                .into())
            }
        };
        parser::bump(ps, self);
        Ok(Term {
            span: name_tok.span,
            kind: Kind::Extended(TypeAliasKind::StructIdent(name_val)),
        })
    }

    fn parser_ty_bump_if(&mut self, ps: &mut ParserState<TypeAliasDialect>) -> bool {
        matches!(
            &ps.token.kind,
            TokenKind::Extended(TypeAliasTokenKind::TypeBindingVar(_))
        )
    }

    fn parser_ty(&mut self, ps: &mut ParserState<TypeAliasDialect>) -> Result<Type<TypeAliasDialect>> {
        let binding = ps.token.kind.clone();

        let TokenKind::Extended(TypeAliasTokenKind::TypeBindingVar(type_decl_key)) = binding else {
            return Err(Error {
                span: ps.span,
                tok: ps.token.clone(),
                kind: ErrorKind::ExtendedError::<TypeAliasTokenKind>(format!(
                    "Expected a TypeBindingVar, got {:?}",
                    binding
                )),
            }
            .into());
        };
        parser::bump(ps, self);

        let applied_types = if ps.token.kind == TokenKind::LSquare {
            pulls_types_from_tyapp(ps, self)?
        } else {
            Vec::new()
        };

        let type_alias_app = Type::Extended(TypeAliasType::TypeAliasApp(type_decl_key, applied_types));
        Ok(type_alias_app)
    }

    fn pat_ctor_eq_within(
        &self,
        ctx: &mut TypeChecker<TypeAliasDialect>,
        ctor_label: &str,
        inner: &Pattern<TypeAliasDialect>,
        aliased_target: &<TypeAliasDialect as SystemRDialect>::Type,
    ) -> bool {
        let TypeAliasType::TypeAliasApp(type_alias_label, inner_types) = aliased_target else {
            return false;
        };

        let ps = &mut ctx.ext_state;
        let dealiased = match reify_type(ps, type_alias_label, inner_types) {
            Err(e) => return false,
            Ok(t) => t,
        };

        // NOW it should be a variant
        match dealiased {
            Type::Variant(v) => {
                for discriminant in v {
                    if ctor_label == discriminant.label && ctx.pattern_type_eq(inner, &discriminant.ty, self) {
                        return true;
                    }
                }
                false
            }
            v => false,
        }
    }

    fn type_check_injection_to_ext(
        &mut self,
        ctx: &mut TypeChecker<TypeAliasDialect>,
        inj_label: &str,
        target: &<TypeAliasDialect as SystemRDialect>::Type,
        tm: &Term<TypeAliasDialect>,
    ) -> Result<Type<TypeAliasDialect>> {
        let sp = tm.span;
        let TypeAliasType::TypeAliasApp(type_alias_label, inner_types) = target else {
            return Err(Diagnostic::error(sp, String::new()).into());
        };

        let ps = &mut ctx.ext_state;
        let dealiased = match reify_type(ps, type_alias_label, inner_types) {
            Err(e) => return Err(Diagnostic::error(sp, format!("failed to reify type: {:?}", e)).into()),
            Ok(t) => t,
        };

        // NOW it should be a variant
        match dealiased.clone() {
            Type::Variant(fields) => {
                for f in fields.clone() {
                    if inj_label == f.label {
                        let ty = ctx.type_check(tm, self)?;
                        if ty == f.ty {
                            return Ok(dealiased);
                        }
                    }
                }
                Err(Diagnostic::error(
                    sp,
                    format!(
                        "constructor {} does not belong to the variant {:?}",
                        inj_label,
                        fields
                            .iter()
                            .map(|f| f.label.clone())
                            .collect::<Vec<String>>()
                            .join(" | ")
                    ),
                )
                .into())
            }
            v => Err(Diagnostic::error(sp, format!("expected de-aliased type to be a Varient, was {:?}", v)).into()),
        }
    }

    fn pat_visit_constructor_of_ext(
        &mut self,
        ext_state: &TypeAliasDialectState,
        pts: &mut crate::patterns::PatTyStack<TypeAliasDialect>,
        label: &str,
        pat: &Pattern<TypeAliasDialect>,
        ext_ty: &<TypeAliasDialect as SystemRDialect>::Type,
    ) {
        // handle where this is Type::Extended; at which point
        // the extension needs to re-create the above logic (including variant_field())
        let ty = pts.ty.clone();

        let TypeAliasType::TypeAliasApp(ext_label, applied_types) = ext_ty else {
            return;
        };
        let reified_type = match reify_type(ext_state, ext_label, applied_types) {
            Ok(t) => t,
            Err(e) => {
                panic!("reifiy failed {:?}", e);
            }
        };
        pts.ty = reified_type.clone();

        pts.visit_constructor(label, pat, self, ext_state);

        pts.ty = ty;
    }

    fn exhaustive_for_ext(
        &mut self,
        matrix: &crate::type_check::patterns::Matrix<TypeAliasDialect>,
        ext_state: &mut <TypeAliasDialect as SystemRDialect>::DialectState,
    ) -> bool {
        let Type::Extended(TypeAliasType::TypeAliasApp(type_decl_key, applied_types)) = matrix.expr_ty.clone() else {
            return false;
        };
        let reified = match reify_type(ext_state, &type_decl_key, &applied_types) {
            Ok(t) => t,
            _ => return false,
        };
        match reified {
            Type::Variant(v) => v.iter().all(|variant| {
                // For all constructors in the sum type, generate a constructor
                // pattern that will match all possible inhabitants of that
                // constructor
                let con = Pattern::Constructor(variant.label.clone(), Box::new(Pattern::Any));
                let temp = [&con];
                let mut ret = false;
                for row in &matrix.inner_matrix {
                    if row.iter().zip(&temp).all(|(a, b)| overlap::<TypeAliasDialect>(a, b)) {
                        ret = true;
                        break;
                    }
                }
                ret
            }),
            _ => false,
        }
    }

    fn ty_subst_visit_ext(
        &mut self,
        subst_visitor: &mut Subst<TypeAliasDialect>,
        ext_ty: &mut Type<TypeAliasDialect>,
        ext_state: &<TypeAliasDialect as SystemRDialect>::DialectState,
    ) {
        let Type::Extended(TypeAliasType::TypeAliasApp(type_decl_key, applied_types)) = ext_ty else {
            return;
        };
        let reified = match reify_type(ext_state, type_decl_key, applied_types) {
            Ok(t) => t,
            _ => return,
        };
        *ext_ty = reified;
        subst_visitor.visit(ext_ty, self, ext_state);
    }

    fn ty_aliaser_visit_ext(
        &mut self,
        aliaser: &mut crate::type_check::Aliaser<TypeAliasDialect>,
        ext_ty: &mut Type<TypeAliasDialect>,
        ext_state: &<TypeAliasDialect as SystemRDialect>::DialectState,
    ) {
        let Type::Extended(TypeAliasType::TypeAliasApp(type_decl_key, applied_types)) = ext_ty.clone() else {
            return;
        };
        let mut reified = match reify_type(ext_state, &type_decl_key, &applied_types) {
            Ok(t) => t,
            _ => return,
        };
        aliaser.visit(&mut reified, self, ext_state);
    }

    fn ty_shift_visit_ext(
        &mut self,
        shift: &mut crate::type_check::visit::Shift,
        ext_ty: &mut Type<TypeAliasDialect>,
        ext_state: &<TypeAliasDialect as SystemRDialect>::DialectState,
    ) {
        let Type::Extended(TypeAliasType::TypeAliasApp(type_decl_key, applied_types)) = ext_ty.clone() else {
            return;
        };
        let reified = match reify_type(ext_state, &type_decl_key, &applied_types) {
            Ok(t) => t,
            _ => return,
        };
        shift.cutoff += 1;
        *ext_ty = reified; // :3
        shift.visit(ext_ty, self, ext_state);
        shift.cutoff -= 1;
    }

    fn type_check_ext_equals_ty(
        &mut self,
        ctx: &mut TypeChecker<TypeAliasDialect>,
        ext_ty: &mut <TypeAliasDialect as SystemRDialect>::Type,
        other_ty: &mut Type<TypeAliasDialect>,
    ) -> bool {
        match ext_ty.clone() {
            TypeAliasType::TypeAliasApp(type_decl_key, applied_types) => {
                let reified = match reify_type(&ctx.ext_state, &type_decl_key, &applied_types) {
                    Ok(t) => t,
                    _ => return false,
                };
                reified == *other_ty
            }
            TypeAliasType::VarWrap(v) => false, // shouldn't happen
            TypeAliasType::Empty => false,      // shouldn't happen
        }
    }
}

pub fn has_holed_type_named(ps: &<TypeAliasDialect as SystemRDialect>::DialectState, key: &str) -> Result<bool> {
    Ok(get_holed_type_from(ps, key).is_ok())
}

pub fn get_holed_type_from(ps: &TypeAliasDialectState, key: &str) -> Result<(usize, Type<TypeAliasDialect>)> {
    match ps.type_map.get(key) {
        Some(t) => Ok(t.clone()),
        None => Err(Error {
            span: Default::default(),
            tok: Default::default(), // FIXME replace span/tok defaults and propagating it into extensions
            kind: ErrorKind::ExtendedError::<TypeAliasTokenKind>(format!(
                "Tried to get holed type named '{:?}', found None",
                key
            )),
        }
        .into()),
    }
}

pub fn set_holed_type_for(
    ps: &mut ParserState<TypeAliasDialect>,
    key: &str,
    ty: (usize, Type<TypeAliasDialect>),
) -> Result<()> {
    match ps.ext_state.type_map.contains_key(key) {
        false => {
            ps.ext_state.type_map.insert(key.to_owned(), ty);
            Ok(())
        }
        true => Err(Error {
            span: ps.span,
            tok: ps.token.clone(),
            kind: ErrorKind::ExtendedError::<TypeAliasTokenKind>(format!(
                "key {:?} is already present in types hash",
                key
            )),
        }
        .into()),
    }
}

pub fn reify_type(
    ext_state: &TypeAliasDialectState,
    type_decl_key: &str,
    applied_types: &Vec<Type<TypeAliasDialect>>,
) -> Result<Type<TypeAliasDialect>> {
    let (tyabs_count, mut holed_type) = get_holed_type_from(ext_state, type_decl_key)?;

    let mut ext = TypeAliasExtension; // :3

    if applied_types.len() != tyabs_count {
        return Err(Error {
            span: Default::default(),
            tok: Default::default(),
            kind: ErrorKind::ExtendedError::<TypeAliasTokenKind>(format!(
                "Expected tyabs count in holed type of be {:?}, but was {:?}",
                applied_types.len(),
                tyabs_count
            )),
        }
        .into());
    }

    let cutoffs = (0..applied_types.len()).rev();
    let mut var_indexes_to_fix = Vec::new();
    for (ty, desired_cutoff) in applied_types.iter().zip(cutoffs) {
        // FIXME error if there's no substitution
        let mut working_ty = ty.clone();
        if let Type::Var(v) = working_ty {
            var_indexes_to_fix.push(v);
            working_ty = Type::Extended(TypeAliasType::VarWrap(v))
        }
        let abstract_type = holed_type.clone();
        let mut subst_visitor = Subst::new(working_ty.clone());
        subst_visitor.cutoff = desired_cutoff;
        subst_visitor.visit(&mut holed_type, &mut ext, ext_state);
    }
    holed_type = VarWrapReplacingVisitor.visit(&holed_type);

    Ok(holed_type)
}

pub fn pulls_types_from_tyapp(
    ps: &mut ParserState<TypeAliasDialect>,
    ext: &mut TypeAliasExtension,
) -> Result<Vec<Type<TypeAliasDialect>>> {
    parser::expect(ps, ext, TokenKind::LSquare)?;

    let mut ret_val = Vec::new();
    loop {
        let ty = parser::ty(ps, ext)?;
        ret_val.push(ty);

        if ps.token.kind == TokenKind::RSquare {
            break;
        }
        if ps.token.kind == TokenKind::Comma {
            parser::bump(ps, ext);
            continue;
        }
        return Err(Error {
            span: ps.span,
            tok: ps.token.clone(),
            kind: ErrorKind::ExtendedError("Expected either end of tyapp or comma".to_owned()),
        }
        .into());
    }

    parser::expect(ps, ext, TokenKind::RSquare)?;
    Ok(ret_val)
}

pub fn extract_tyabs_for_type_shape(
    ps: &mut ParserState<'_, TypeAliasDialect>,
    ext: &mut TypeAliasExtension,
) -> Result<usize> {
    let mut ext_2 = *ext;
    let mut tyabs = Vec::new();
    if ps.token.kind == TokenKind::Lambda {
        parser::expect(ps, ext, TokenKind::Lambda)?;
        tyabs = parser::once_or_more(
            ps,
            |p| {
                let tyvar = parser::uppercase_id(p, ext)?;
                p.tyvar.push(tyvar);
                Ok(())
            },
            TokenKind::Lambda,
            &mut ext_2,
        )?;
    }
    Ok(tyabs.len()) // FIXME do dynamic tyabs extraction and return the count
}

pub fn parse_holed_type_from_decl(
    ps: &mut ParserState<'_, TypeAliasDialect>,
    ext: &mut TypeAliasExtension,
) -> Result<(usize, Type<TypeAliasDialect>)> {
    let sp = ps.span;
    let push_count = extract_tyabs_for_type_shape(ps, ext)?;

    // type def
    let type_shape = parser::once(
        ps,
        |p| parser::ty_atom(p, &mut TypeAliasExtension),
        "abstraction body required",
    )?;

    // pop off the tyabs var, since they only apply for the purpose of defining
    // "type holes" in the type def
    for _ in 0..push_count {
        ps.tyvar.pop();
    }

    Ok((push_count, type_shape))
}

pub struct TypeAliasToBottomDialectResolver;

impl SystemRResolver<TypeAliasDialect, BottomDialect> for TypeAliasToBottomDialectResolver {
    fn resolve(&self, ext_state: TypeAliasDialectState, tm: Term<TypeAliasDialect>) -> Result<Term<BottomDialect>> {
        //let out_tm = Term::unit();
        let ttv = TatbTermVisitor::new(ext_state);
        Ok(ttv.visit(&tm))
    }
}

pub struct TatbPatVisitor {
    ext_state: Rc<RefCell<TypeAliasDialectState>>,
}

impl DialectChangingPatternVisitor<TypeAliasDialect, BottomDialect> for TatbPatVisitor {
    fn visit_ext(&self, pat: &Pattern<TypeAliasDialect>) -> Pattern<BottomDialect> {
        panic!("pattern visit_ext not impl, {:?}", self.ext_state.as_ref())
    }
}

pub struct TatbTypeVisitor {
    ext_state: Rc<RefCell<TypeAliasDialectState>>,
}

pub struct VarWrapReplacingVisitor;
impl DialectChangingTypeVisitor<TypeAliasDialect, TypeAliasDialect> for VarWrapReplacingVisitor {
    fn visit_ext(&self, ty: &Type<TypeAliasDialect>) -> Type<TypeAliasDialect> {
        match ty {
            Type::Extended(TypeAliasType::VarWrap(v)) => Type::Var(*v),
            v => v.clone(),
        }
    }
}

impl DialectChangingTypeVisitor<TypeAliasDialect, BottomDialect> for TatbTypeVisitor {
    fn visit_ext(&self, ty: &Type<TypeAliasDialect>) -> Type<BottomDialect> {
        match ty {
            Type::Extended(TypeAliasType::TypeAliasApp(label, applied_types)) => {
                let ext_state = &*self.ext_state.as_ref().borrow();
                let out_ty = match reify_type(ext_state, label, applied_types) {
                    Ok(t) => t,
                    Err(e) => panic!("failed called reifiy_type with error: {:?}", e),
                };
                self.visit(&out_ty)
            }
            _ => panic!("called into visit_ext for Tatb Type visitor with non TypeAliasApp type; shouldn't happen"),
        }
    }
}

pub struct TatbTermVisitor {
    pub ext_state: Rc<RefCell<TypeAliasDialectState>>,
}

impl TatbTermVisitor {
    pub fn new(ext_state: TypeAliasDialectState) -> Self {
        TatbTermVisitor {
            ext_state: Rc::new(RefCell::new(ext_state)),
        }
    }
}

impl DialectChangingTermVisitor<TypeAliasDialect, BottomDialect, TatbTypeVisitor, TatbPatVisitor> for TatbTermVisitor {
    fn get_type_visitor(&self) -> TatbTypeVisitor {
        TatbTypeVisitor {
            ext_state: self.ext_state.clone(),
        }
    }

    fn get_pat_visitor(&self) -> TatbPatVisitor {
        TatbPatVisitor {
            ext_state: self.ext_state.clone(),
        }
    }

    fn visit_ext(&self, term: &Term<TypeAliasDialect>) -> Term<BottomDialect> {
        panic!("no extended kinds should have persisted beyond parse phase");
    }
}
