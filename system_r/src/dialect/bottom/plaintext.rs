use super::BottomDialect;
use crate::{
    dialect::{SystemRDialect, SystemRExtension},
    terms::{plaintext::Plaintext, Kind, Literal, Primitive, Term}, type_check::Type,
};
use anyhow::Result;

const TYVAR_FLOOR_CHAR: char = 'A';
const VAR_FLOOR_CHAR: char = 'a';
const TYVAR_CEILING: u32 = 90;
const VAR_CEILING: u32 = 122;

fn wrap_for_ceiling(char_pos: u32, ceiling: u32) -> bool {
    char_pos >= ceiling
}


fn add1_char(c: char) -> char {
    std::char::from_u32(c as u32 + 1).unwrap_or(c)
}

fn do_debruijn_mangling_within(last_name: &str, floor_char: char, ceiling: u32) -> String {
    let curr_len = last_name.len();
    let last_char = last_name.chars().last().unwrap();
    if wrap_for_ceiling(last_char as u32, ceiling) {
        let next_char = floor_char; // 60
        let partial = last_name.chars().into_iter().take(last_name.len() - 1).map(|f| {
            if wrap_for_ceiling(add1_char(f) as u32, ceiling) {
                floor_char
            } else {
                add1_char(f)
            }
        }).collect::<String>();
        let next_name = partial + &next_char.to_string() + &floor_char.to_string();
        return next_name
    } else {
        let partial = last_name.chars().into_iter().take(last_name.len() - 1).collect::<String>();
        let next_name = partial + &add1_char(last_char).to_string();
    }
    String::new()
}

fn generic_var_getter(names_debruijn: &mut Vec<String>, floor_char: char, ceiling: u32) -> String {
    let next_var = if names_debruijn.is_empty() {
        floor_char.to_string()
    } else {
        let last_name = names_debruijn.last().unwrap();
        let next_var = do_debruijn_mangling_within(last_name, floor_char, ceiling);
        next_var
    };
    names_debruijn.push(next_var.clone());
    next_var
}

fn get_next_var(names_debruijn: &mut Vec<String>) -> String {
    generic_var_getter(names_debruijn, VAR_FLOOR_CHAR, VAR_CEILING)
}

fn get_next_tyvar(names_debruijn: &mut Vec<String>) -> String {
    generic_var_getter(names_debruijn, TYVAR_FLOOR_CHAR, TYVAR_CEILING)
}

pub fn term_to_plaintext<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    input: &Term<TExtDialect>,
    ext: &TExt,
    vars_debruijn: &mut Vec<String>,
    tyvars_debruijn: &mut Vec<String>,
) -> Result<String> {
    match input.kind.clone() {
        Kind::Lit(v) => Ok(vanilla_lit_kind_to_plaintext::<TExtDialect>(&v)),
        crate::terms::Kind::Var(idx) => 
            var_to_plaintext(idx, ext, vars_debruijn, tyvars_debruijn),
        crate::terms::Kind::PlatformBinding(_) => todo!("platformbinding"),
        crate::terms::Kind::Fix(_) => todo!("fix"),
        crate::terms::Kind::Primitive(name) => Ok(primitive_to_plaintext(&name)),
        crate::terms::Kind::Injection(_, _, _) => todo!("inj"),
        crate::terms::Kind::Product(_) => todo!("product"),
        crate::terms::Kind::Projection(_, _) => todo!("proj"),
        crate::terms::Kind::Case(_, _) => todo!("case"),
        crate::terms::Kind::Let(pat, val, scope) =>
            let_block_to_plaintext(pat, val, scope, vars_debruijn, tyvars_debruijn),
        crate::terms::Kind::Abs(arg_ty, body) => 
            function_abstraction_to_plaintext(arg_ty, body, ext, vars_debruijn, tyvars_debruijn),
        crate::terms::Kind::App(l, r) =>
            application_to_plaintext(&l, &r, ext, vars_debruijn, tyvars_debruijn),
        crate::terms::Kind::TyAbs(inner_term) =>
            type_abstraction_to_plaintext(inner_term, ext, vars_debruijn, tyvars_debruijn),
        crate::terms::Kind::TyApp(_, _) => todo!("tyapp"),
        crate::terms::Kind::Fold(_, _) => todo!("fold"),
        crate::terms::Kind::Unfold(_, _) => todo!("unfold"),
        crate::terms::Kind::Pack(_, _, _) => todo!("pack"),
        crate::terms::Kind::Unpack(_, _) => todo!("unpack"),
        k @ Kind::Extended(_) => ext.to_plaintext(input),
    }
}

fn let_block_to_plaintext(
        pat: Box<Pattern<TExtDialect>
    ) -> Result<String> {
    Ok("".to_owned())
}

fn var_to_plaintext<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
        idx: usize,
        ext: &TExt,
        vars_debruijn: &mut Vec<String>,
        _tyvars_debruijn: &mut Vec<String>,
    ) -> Result<String> {
        let idx = 0;
        Ok(vars_debruijn[idx].clone())
    }

fn type_shape_to_plaintext<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
        ty: &Type<TExtDialect>,
        ext: &TExt,
        vars_debruijn: &mut Vec<String>,
        tyvars_debruijn: &mut Vec<String>,
    ) -> Result<String> {
        match ty {
            Type::Unit => Ok("Unit".to_owned()),
            Type::Nat => Ok("Nat".to_owned()),
            Type::Bool => Ok("Bool".to_owned()),
            Type::Bytes => Ok("Bytes".to_owned()),
            Type::Tag(_) => todo!(),
            Type::Alias(_) => todo!(),
            Type::Var(tyvar_idx) => Ok(tyvars_debruijn[tyvars_debruijn.len() - (1 + tyvar_idx)].clone()),
            Type::Variant(_) => todo!(),
            Type::Product(_) => todo!(),
            Type::PlatformBinding(_, _) => todo!(),
            Type::Arrow(_, _) => todo!(),
            Type::Universal(_) => todo!(),
            Type::Existential(_) => todo!(),
            Type::Rec(_) => todo!(),
            Type::Extended(_) => todo!(),
        }
    }

fn function_abstraction_to_plaintext<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
        arg_ty: Box<Type<TExtDialect>>,
        body: Box<Term<TExtDialect>>,
        ext: &TExt,
        vars_debruijn: &mut Vec<String>,
        tyvars_debruijn: &mut Vec<String>
    ) -> std::prelude::v1::Result<String, anyhow::Error> {
    let next_var = get_next_var(vars_debruijn);
    let inner_plaintext = term_to_plaintext(&body, ext, vars_debruijn, tyvars_debruijn)?;
    let ty_plaintext = type_shape_to_plaintext(&arg_ty, ext, vars_debruijn, tyvars_debruijn)?;
    Ok(format!("\\{next_var}: {ty_plaintext}. {inner_plaintext}"))
}

fn type_abstraction_to_plaintext<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
        inner_term: Box<Term<TExtDialect>>,
        ext: &TExt,
        vars_debruijn: &mut Vec<String>,
        tyvars_debruijn: &mut Vec<String>
    ) -> std::prelude::v1::Result<String, anyhow::Error> {
    let next_tyvar = get_next_tyvar(tyvars_debruijn);
    let inner_plaintext = term_to_plaintext(&inner_term, ext, vars_debruijn, tyvars_debruijn)?;
    Ok(format!("\\{next_tyvar} {inner_plaintext}"))
}

fn application_to_plaintext<TExtDialect: SystemRDialect, TExt: SystemRExtension<TExtDialect>>(
    lhand: &Box<Term<TExtDialect>>,
    rhand: &Box<Term<TExtDialect>>,
    ext: &TExt,
    vars_debruijn: &mut Vec<String>,
    tyvars_debruijn: &mut Vec<String>) -> Result<String> {
        let lstr = term_to_plaintext(lhand, ext, vars_debruijn, tyvars_debruijn)?;
        let rstr = term_to_plaintext(rhand, ext, vars_debruijn, tyvars_debruijn)?;
        Ok(format!("{lstr} {rstr}"))
}

fn primitive_to_plaintext(prim: &Primitive) -> String {
    match prim {
        Primitive::Succ => "succ".to_owned(),
        Primitive::Pred => "pred".to_owned(),
        Primitive::IsZero => "iszero".to_owned(),
    }
}

fn vanilla_lit_kind_to_plaintext<TExtDialect: SystemRDialect>(input: &Literal) -> String {
    match input {
        Literal::Unit => "Unit".to_owned(),
        Literal::Bool(b) => if b == &true { "true".to_owned() } else { "false".to_owned() },
        Literal::Nat(n) => n.to_string(),
        Literal::Tag(t) => format!("{t}"),
        Literal::Bytes(bytes) => {
            let reduced: String = bytes.iter()
                .map(|e| format!("{e}")).collect::<Vec<String>>()
                .join(", ");
            return format!("[{reduced}]");
        },
    }
}

impl Plaintext<BottomDialect> for Term<BottomDialect> {
    fn to_plaintext<TExt: SystemRExtension<BottomDialect>>(&self, ext: &TExt) -> Result<String> {
        let mut vars_debruijn = Vec::new();
        let mut tyvars_debruijn = Vec::new();
        match &self.kind {
            // Any dialect building-up from BottomDialect should implement the extended arm for its
            // respective dialect items, otherwise delegate everything else to vanilla_dialect_to_plaintext(),
            // which is exported and should be the extry point for handing it any term
            &Kind::Extended(_) => Err(anyhow!(
                "Plaintext for Term<BottomDialect>: should never have extended
                kinds, but got one (shouldn't ever happen)"
            )),
            _ => term_to_plaintext(self, ext, &mut vars_debruijn, &mut tyvars_debruijn),
        }
    }
}