//! TypeAliasDialect
/// - Extends the Bottom-dialect of system_r (ie system_f with some minor
///   kind/type enhancement)
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use system_r::{
    dialect::bottom::{BottomDialect, BottomKind, BottomPattern, BottomTokenKind},
    patterns::Pattern,
    syntax::{parser::Parser, TokenKind},
    terms::{Kind, Term},
    type_check::{patterns::overlap, visit::Subst, Type, TypeChecker},
    visit::{
        DialectChangingPatternVisitor, DialectChangingTermVisitor, DialectChangingTypeVisitor, MutTypeVisitor,
        PatternVisitor,
    },
};

use self::type_alias_catalog::{syntax, type_check};

use system_r::dialect::{
    ExtendedDialectState, ExtendedKind, ExtendedPattern, ExtendedTokenKind, ExtendedType, SystemRDialect,
    SystemRExtension, SystemRResolver,
};

use anyhow::Result;

#[derive(Copy, Clone, Debug, Default)]
pub struct TypeAliasExtension;

pub type TypeAliasTypeChecker = TypeChecker<TypeAliasDialect>;

#[derive(Clone, Debug, Default, PartialEq, PartialOrd)]
pub enum TypeAliasTokenKind {
    #[default]
    Placeholder,
    TypeAliasKeyword,
    TypeAliasDeclVarName(String),
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
        TypeAliasTokenKind::TypeAliasDeclVarName(data.to_owned())
    }

    fn lex_ext_keyword(&mut self, data: &str) -> Result<TypeAliasTokenKind> {
        if data == KEYWORD_TYPE {
            return Ok(TypeAliasTokenKind::TypeAliasKeyword);
        }
        Err(anyhow!(
            "called lex_ext_keyword with a data str that wasn't TypeAlias-related; shouldn't happen"
        ))
    }

    fn pat_ext_pattern_type_eq(
        &self,
        _ctx: &TypeChecker<TypeAliasDialect>,
        _pat: &TypeAliasPattern,
        _ty: &system_r::type_check::Type<TypeAliasDialect>,
    ) -> bool {
        false
    }

    fn pat_add_ext_pattern<'a>(
        &'a self,
        _parent: &system_r::type_check::patterns::Matrix<'a, TypeAliasDialect>,
        _ext_pattern: &system_r::patterns::Pattern<TypeAliasDialect>,
    ) -> bool {
        false
    }

    fn pat_ext_matches(&self, _pat: &TypeAliasPattern, _term: &system_r::terms::Term<TypeAliasDialect>) -> bool {
        false
    }

    fn parser_has_ext_parse(&self, tk: &TypeAliasTokenKind) -> Result<bool> {
        Ok(tk == &TypeAliasTokenKind::TypeAliasKeyword)
    }

    fn parser_ext_parse<'s>(&mut self, ps: &mut Parser<TypeAliasDialect>) -> Result<Term<TypeAliasDialect>> {
        ps.expect(self, TokenKind::Extended(TypeAliasTokenKind::TypeAliasKeyword))?;

        let struct_ident = ps.once(|p| p.atom(self), "missing struct identifier $Binding")?;
        let struct_ident = match struct_ident.kind {
            Kind::Extended(TypeAliasKind::StructIdent(s)) => s,
            e => return Err(syntax::err_02_expected_struct_ident(&ps.span, &ps.token, &e).into()),
        };

        ps.expect(self, TokenKind::Equals)?;

        // this should be a "holed-out" type specification (it should have
        // entries for where generic types can be substituted-in)
        let type_shape = parse_holed_type_from_decl(ps, self)?;
        set_holed_type_for(ps, &struct_ident, type_shape.clone())?;

        let len = ps.tmvar.len();
        ps.expect(self, TokenKind::In)?;

        let t2 = ps.once(|p| p.parse(self), "type scope body required")?;
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

    fn parser_has_ext_atom(&self, _tk: &TypeAliasTokenKind) -> bool {
        false
    }

    fn parser_ext_atom(&mut self, ps: &mut Parser<'_, TypeAliasDialect>) -> Result<Term<TypeAliasDialect>> {
        let name_tok = ps.token.clone();
        let name_val = match name_tok.kind.clone() {
            TokenKind::Extended(n) => match n {
                TypeAliasTokenKind::TypeAliasDeclVarName(name) => name,
                v => return Err(syntax::err_03_expected_typealiasdeclvarname(&name_tok.span, &name_tok, &v).into()),
            },
            v => return Err(syntax::err_04_expected_extendedtokenkind(&name_tok.span, &name_tok, &v).into()),
        };
        ps.bump(self);
        Ok(Term {
            span: name_tok.span,
            kind: Kind::Extended(TypeAliasKind::StructIdent(name_val)),
        })
    }

    fn parser_ty_bump_if(&mut self, ps: &mut Parser<TypeAliasDialect>) -> bool {
        matches!(
            &ps.token.kind,
            TokenKind::Extended(TypeAliasTokenKind::TypeAliasDeclVarName(_))
        )
    }

    fn parser_ty(&mut self, ps: &mut Parser<TypeAliasDialect>) -> Result<Type<TypeAliasDialect>> {
        let ty_decl = ps.token.kind.clone();

        let TokenKind::Extended(TypeAliasTokenKind::TypeAliasDeclVarName(type_decl_key)) = ty_decl else {
            return Err(syntax::err_04_expected_extendedtokenkind(&ps.span, &ps.token, &ty_decl).into());
        };
        ps.bump(self);

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
            Err(_) => return false,
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
            _ => false,
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
            return Err(type_check::err_01(sp, target).into());
        };

        let ps = &mut ctx.ext_state;
        let dealiased = match reify_type(ps, type_alias_label, inner_types) {
            Err(e) => return Err(type_check::err_02(sp, &e).into()),
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
                Err(type_check::err_03(
                    sp,
                    inj_label,
                    fields
                        .iter()
                        .map(|f| f.label.clone())
                        .collect::<Vec<String>>()
                        .join(" | "),
                )
                .into())
            }
            v => Err(type_check::err_04(sp, &v).into()),
        }
    }

    fn pat_visit_constructor_of_ext(
        &mut self,
        ext_state: &TypeAliasDialectState,
        pts: &mut system_r::patterns::PatTyStack<TypeAliasDialect>,
        label: &str,
        pat: &Pattern<TypeAliasDialect>,
        ext_ty: &<TypeAliasDialect as SystemRDialect>::Type,
    ) -> Result<()> {
        let ty = pts.ty.clone();

        let TypeAliasType::TypeAliasApp(ext_label, applied_types) = ext_ty else {
            return Err(type_check::err_06_expected_type_alias_app(Type::Extended(ext_ty.clone())).into());
        };
        let reified_type = reify_type(ext_state, ext_label, applied_types)?;
        pts.ty = reified_type.clone();

        pts.visit_constructor(label, pat, self, ext_state)?;

        pts.ty = ty;
        Ok(())
    }

    fn exhaustive_for_ext(
        &mut self,
        matrix: &system_r::type_check::patterns::Matrix<TypeAliasDialect>,
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
    ) -> Result<()> {
        let Type::Extended(TypeAliasType::TypeAliasApp(type_decl_key, applied_types)) = ext_ty else {
            if let Type::Extended(TypeAliasType::VarWrap(_)) = ext_ty {
                return Ok(()); // blow past these; they are handled later
            }
            // otherwise this is an error
            return Err(type_check::err_06_expected_type_alias_app(ext_ty.clone()).into());
        };
        let reified = match reify_type(ext_state, type_decl_key, applied_types) {
            Ok(t) => t,
            _ => return Ok(()),
        };
        *ext_ty = reified;
        subst_visitor.visit(ext_ty, self, ext_state)
    }

    fn ty_aliaser_visit_ext(
        &mut self,
        aliaser: &mut system_r::type_check::Aliaser<TypeAliasDialect>,
        ext_ty: &mut Type<TypeAliasDialect>,
        ext_state: &<TypeAliasDialect as SystemRDialect>::DialectState,
    ) -> Result<()> {
        let Type::Extended(TypeAliasType::TypeAliasApp(type_decl_key, applied_types)) = ext_ty.clone() else {
            return Err(type_check::err_06_expected_type_alias_app(ext_ty.clone()).into());
        };
        let mut reified = reify_type(ext_state, &type_decl_key, &applied_types)?;
        aliaser.visit(&mut reified, self, ext_state)
    }

    fn ty_shift_visit_ext(
        &mut self,
        shift: &mut system_r::type_check::visit::Shift,
        ext_ty: &mut Type<TypeAliasDialect>,
        ext_state: &<TypeAliasDialect as SystemRDialect>::DialectState,
    ) -> Result<()> {
        let Type::Extended(TypeAliasType::TypeAliasApp(type_decl_key, applied_types)) = ext_ty.clone() else {
            if let Type::Extended(TypeAliasType::VarWrap(_)) = ext_ty {
                return Ok(()); // blow past these; they are handled later
            }
            // otherwise this is an error
            return Err(type_check::err_06_expected_type_alias_app(ext_ty.clone()).into());
        };
        let reified = reify_type(ext_state, &type_decl_key, &applied_types)?;
        shift.cutoff += 1;
        *ext_ty = reified; // :3
        shift.visit(ext_ty, self, ext_state)?;
        shift.cutoff -= 1;
        Ok(())
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
            TypeAliasType::VarWrap(_) => false, // shouldn't happen
            TypeAliasType::Empty => false,      // shouldn't happen
        }
    }

    fn to_plaintext(&self, _input: &Term<TypeAliasDialect>) -> Result<String> {
        todo!()
    }
}

pub fn get_holed_type_from(ps: &TypeAliasDialectState, key: &str) -> Result<(usize, Type<TypeAliasDialect>)> {
    match ps.type_map.get(key) {
        Some(t) => Ok(t.clone()),
        None => Err(syntax::err_05_expected_some_holed_type(key).into()),
    }
}

pub fn set_holed_type_for(
    ps: &mut Parser<TypeAliasDialect>,
    key: &str,
    ty: (usize, Type<TypeAliasDialect>),
) -> Result<()> {
    match ps.ext_state.type_map.contains_key(key) {
        false => {
            ps.ext_state.type_map.insert(key.to_owned(), ty);
            Ok(())
        }
        true => Err(syntax::err_06_expected_no_entry_for_key(&ps.span, &ps.token, key).into()),
    }
}

pub fn reify_type(
    ext_state: &TypeAliasDialectState,
    type_decl_key: &str,
    applied_types: &[Type<TypeAliasDialect>],
) -> Result<Type<TypeAliasDialect>> {
    let (tyabs_count, mut holed_type) = get_holed_type_from(ext_state, type_decl_key)?;

    let mut ext = TypeAliasExtension; // :3

    if applied_types.len() != tyabs_count {
        return Err(type_check::err_05(applied_types.len(), tyabs_count).into());
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
        let mut subst_visitor = Subst::new(working_ty.clone());
        subst_visitor.cutoff = desired_cutoff;
        subst_visitor.visit(&mut holed_type, &mut ext, ext_state)?;
    }
    holed_type = VarWrapReplacingVisitor.visit(&holed_type)?;

    Ok(holed_type)
}

pub fn pulls_types_from_tyapp(
    ps: &mut Parser<TypeAliasDialect>,
    ext: &mut TypeAliasExtension,
) -> Result<Vec<Type<TypeAliasDialect>>> {
    ps.expect(ext, TokenKind::LSquare)?;

    let mut ret_val = Vec::new();
    loop {
        let ty = ps.ty(ext)?;
        ret_val.push(ty);

        if ps.token.kind == TokenKind::RSquare {
            break;
        }
        if ps.token.kind == TokenKind::Comma {
            ps.bump(ext);
            continue;
        }
        return Err(syntax::err_01_expected_end_of_tyapp_or_comma(&ps.span, &ps.token).into());
    }

    ps.expect(ext, TokenKind::RSquare)?;
    Ok(ret_val)
}

pub fn extract_tyabs_for_type_shape(
    ps: &mut Parser<'_, TypeAliasDialect>,
    ext: &mut TypeAliasExtension,
) -> Result<usize> {
    let mut ext_2 = *ext;
    let mut tyabs = Vec::new();
    if ps.token.kind == TokenKind::Lambda {
        ps.expect(ext, TokenKind::Lambda)?;
        tyabs = ps.once_or_more(
            |p| {
                let tyvar = p.uppercase_id(ext)?;
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
    ps: &mut Parser<'_, TypeAliasDialect>,
    ext: &mut TypeAliasExtension,
) -> Result<(usize, Type<TypeAliasDialect>)> {
    let push_count = extract_tyabs_for_type_shape(ps, ext)?;

    // type def
    let type_shape = ps.once(|p| p.ty_atom(&mut TypeAliasExtension), "abstraction body required")?;

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
        ttv.visit(&tm)
    }
}

pub struct TatbPatVisitor {
    ext_state: Rc<RefCell<TypeAliasDialectState>>,
}

impl DialectChangingPatternVisitor<TypeAliasDialect, BottomDialect> for TatbPatVisitor {
    fn visit_ext(&self, _pat: &Pattern<TypeAliasDialect>) -> Result<Pattern<BottomDialect>> {
        Err(anyhow!("pattern visit_ext not impl, {:?}", self.ext_state.as_ref()))
    }
}

pub struct TatbTypeVisitor {
    ext_state: Rc<RefCell<TypeAliasDialectState>>,
}

pub struct VarWrapReplacingVisitor;
impl DialectChangingTypeVisitor<TypeAliasDialect, TypeAliasDialect> for VarWrapReplacingVisitor {
    fn visit_ext(&self, ty: &Type<TypeAliasDialect>) -> Result<Type<TypeAliasDialect>> {
        match ty {
            Type::Extended(TypeAliasType::VarWrap(v)) => Ok(Type::Var(*v)),
            v => Ok(v.clone()),
        }
    }
}

impl DialectChangingTypeVisitor<TypeAliasDialect, BottomDialect> for TatbTypeVisitor {
    fn visit_ext(&self, ty: &Type<TypeAliasDialect>) -> Result<Type<BottomDialect>> {
        let Type::Extended(TypeAliasType::TypeAliasApp(label, applied_types)) = ty else {
            return Err(type_check::err_06_expected_type_alias_app(ty.clone()).into());
        };
        let ext_state = &*self.ext_state.as_ref().borrow();
        let out_ty = reify_type(ext_state, label, applied_types)?;
        self.visit(&out_ty)
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
    fn get_type_visitor(&self) -> Result<TatbTypeVisitor> {
        Ok(TatbTypeVisitor {
            ext_state: self.ext_state.clone(),
        })
    }

    fn get_pat_visitor(&self) -> Result<TatbPatVisitor> {
        Ok(TatbPatVisitor {
            ext_state: self.ext_state.clone(),
        })
    }

    fn visit_ext(&self, _term: &Term<TypeAliasDialect>) -> Result<Term<BottomDialect>> {
        Err(anyhow!("no extended kinds should have persisted beyond parse phase"))
    }
}

mod type_alias_catalog {
    pub mod syntax {
        use crate::type_alias::{TypeAliasDialect, TypeAliasTokenKind};
        use system_r::{
            feedback::{ErrorKind, ExtendedPhaseContent, FeedbackPhase, FeedbackSeverity, SystemRFeedback},
            syntax::{Token, TokenKind},
            terms::Kind,
            util::span::Span,
        };

        fn build_error(
            feedback_code: &str,
            target_span: Span,
            tok: &Token<TypeAliasTokenKind>,
            kind: &ErrorKind<TypeAliasTokenKind>,
            formatted_err_msg: Option<String>,
        ) -> SystemRFeedback<TypeAliasDialect> {
            SystemRFeedback {
                feedback_code: feedback_code.to_owned(),
                target_span: if target_span == Default::default() {
                    None
                } else {
                    Some(target_span)
                },
                phase: FeedbackPhase::Extended {
                    dialect: "TypeAlias".to_owned(),
                    phase: ExtendedPhaseContent::Parse(tok.clone(), kind.clone()),
                },
                severity: FeedbackSeverity::Error(formatted_err_msg.unwrap_or_default()),
                src: None,
                acknowledged: false,
            }
        }

        /// TADPS01
        pub fn err_01_expected_end_of_tyapp_or_comma(
            target_span: &Span,
            tok: &Token<TypeAliasTokenKind>,
        ) -> SystemRFeedback<TypeAliasDialect> {
            build_error(
                "TADPS01",
                *target_span,
                tok,
                &ErrorKind::ExtendedError("Expected either end of tyapp or comma".to_owned()),
                None,
            )
        }

        /// TADPS02
        pub fn err_02_expected_struct_ident(
            target_span: &Span,
            tok: &Token<TypeAliasTokenKind>,
            e: &Kind<TypeAliasDialect>,
        ) -> SystemRFeedback<TypeAliasDialect> {
            build_error(
                "TADPS02",
                *target_span,
                tok,
                &ErrorKind::ExtendedError::<TypeAliasTokenKind>(format!("expected a StructIdent, got {:?}", e)),
                None,
            )
        }

        /// TADPS03
        pub fn err_03_expected_typealiasdeclvarname(
            target_span: &Span,
            tok: &Token<TypeAliasTokenKind>,
            v: &TypeAliasTokenKind,
        ) -> SystemRFeedback<TypeAliasDialect> {
            build_error(
                "TADPS03",
                *target_span,
                tok,
                &ErrorKind::ExtendedError::<TypeAliasTokenKind>(format!("Expected a TypeBindingVar, got {:?}", v)),
                None,
            )
        }

        /// TADPS04
        pub fn err_04_expected_extendedtokenkind(
            target_span: &Span,
            tok: &Token<TypeAliasTokenKind>,
            v: &TokenKind<TypeAliasTokenKind>,
        ) -> SystemRFeedback<TypeAliasDialect> {
            build_error(
                "TADPS04",
                *target_span,
                tok,
                &ErrorKind::ExtendedError::<TypeAliasTokenKind>(format!("Expected a ExtendedTokenKind, got {:?}", v)),
                None,
            )
        }

        /// TADPS05
        pub fn err_05_expected_some_holed_type(key: &str) -> SystemRFeedback<TypeAliasDialect> {
            build_error(
                "TADPS05",
                Default::default(),
                &Default::default(),
                &ErrorKind::ExtendedError::<TypeAliasTokenKind>(format!(
                    "Tried to get holed type named '{:?}', found None",
                    key
                )),
                None,
            )
        }

        /// TADPS06
        pub fn err_06_expected_no_entry_for_key(
            target_span: &Span,
            tok: &Token<TypeAliasTokenKind>,
            key: &str,
        ) -> SystemRFeedback<TypeAliasDialect> {
            build_error(
                "TADPS06",
                *target_span,
                tok,
                &ErrorKind::ExtendedError::<TypeAliasTokenKind>(format!(
                    "key {:?} is already present in types hash",
                    key
                )),
                None,
            )
        }
    }

    pub mod type_check {
        use anyhow::Error;

        use system_r::{
            feedback::{ExtendedPhaseContent, FeedbackPhase, FeedbackSeverity, SystemRFeedback},
            type_check::Type,
            util::span::Span,
        };

        use super::super::{TypeAliasDialect, TypeAliasType};

        fn build_error(
            feedback_code: &str,
            target_span: Span,
            formatted_err_msg: String,
        ) -> SystemRFeedback<TypeAliasDialect> {
            SystemRFeedback {
                feedback_code: feedback_code.to_owned(),
                target_span: if target_span == Default::default() {
                    None
                } else {
                    Some(target_span)
                },
                phase: FeedbackPhase::Extended {
                    dialect: "TypeAlias".to_owned(),
                    phase: ExtendedPhaseContent::Named("TypeCheck".to_owned()),
                },
                severity: FeedbackSeverity::Error(formatted_err_msg),
                src: None,
                acknowledged: false,
            }
        }

        // TADTC01
        pub fn err_01(sp: Span, input: &TypeAliasType) -> SystemRFeedback<TypeAliasDialect> {
            build_error(
                "TADTC01",
                sp,
                format!(
                    r#"Expected the type being checked to be a TypeAliasApp type, but was {:?}"#,
                    input
                )
                .to_owned(),
            )
        }

        // TADTC02
        pub fn err_02(sp: Span, e: &Error) -> SystemRFeedback<TypeAliasDialect> {
            build_error("TADTC02", sp, format!("failed to reify type: {:?}", e).to_owned())
        }

        // TADTC03
        pub fn err_03(sp: Span, inj_label: &str, fields_joined: String) -> SystemRFeedback<TypeAliasDialect> {
            build_error(
                "TADTC03",
                sp,
                format!(
                    "constructor {} does not belong to the variant {:?}",
                    inj_label, fields_joined
                )
                .to_owned(),
            )
        }

        // TADTC04
        pub fn err_04(sp: Span, v: &Type<TypeAliasDialect>) -> SystemRFeedback<TypeAliasDialect> {
            build_error(
                "TADTC04",
                sp,
                format!("expected de-aliased type to be a Varient, was {:?}", v).to_owned(),
            )
        }

        // TADTC05 -- reify type failure
        pub fn err_05(applied_len: usize, tyabs_len: usize) -> SystemRFeedback<TypeAliasDialect> {
            build_error(
                "TADTC05",
                Default::default(),
                format!(
                    "reify type fail: Expected tyabs count in holed type of be {:?}, but was {:?}",
                    applied_len, tyabs_len
                )
                .to_owned(),
            )
        }

        // TADTC06
        pub fn err_06_expected_type_alias_app(ext_ty: Type<TypeAliasDialect>) -> SystemRFeedback<TypeAliasDialect> {
            build_error(
                "TADTC06",
                Default::default(),
                format!("TypeAliasDialect: within pattern visitor for constructor; expected a $TypeAlias definition, but got {:?}", ext_ty),
                )
        }
    }
}
