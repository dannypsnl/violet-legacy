pub mod error;
use crate::ast::*;
use error::{TyckError, TyckError::*};
use itertools::multizip;
use miette::Result;
use std::collections::HashMap;

type Context = HashMap<String, Type>;

fn unify(mod_file: &ModFile, range: &Range, t1: &Type, t2: &Type) -> Result<(), TyckError> {
    match (&t1, &t2) {
        (Type::Free(c), t2) => {
            // record unification
            Ok(())
        }
        (_, Type::Free(_)) => unify(mod_file, range, t2, t1),
        (Type::Arrow(t11, t12), Type::Arrow(t21, t22)) => {
            for (t1, t2) in multizip((t11, t21)) {
                unify(mod_file, range, t1, t2)?;
            }
            unify(mod_file, range, t12, t22)
        }
        (&t1, &t2) => {
            if t1 != t2 {
                Err(TyMismatch {
                    src: mod_file.as_src(),
                    expected: t2.clone(),
                    actual: t1.clone(),
                    span: range.to_span(),
                })?
            } else {
                Ok(())
            }
        }
    }
}

pub fn check_module(mod_file: &ModFile) -> Result<(), TyckError> {
    let mut ctx = Context::new();
    let mut to_check_list: Vec<&Top> = vec![];
    for top in &mod_file.top_list {
        match top {
            Top::TypeDecl(_, name, ty) => {
                ctx.insert(name.clone(), ty.clone());
            }
            Top::DefineProc(_, _, _, _) => to_check_list.push(top),
            Top::DefineVar(_, _, _) => to_check_list.push(top),
        }
    }
    for to_c in to_check_list {
        match to_c {
            Top::DefineProc(range, name, p_list, body) => {
                let expect_ty = match ctx.get(name) {
                    Some(ty) => ty.clone(),
                    None => Err(IdMissing {
                        src: mod_file.as_src(),
                        name: name.clone(),
                        span: range.to_span(),
                    })?,
                };
                let actual_ty = infer(
                    mod_file,
                    &ctx,
                    &Expr::Lambda(range.clone(), p_list.clone(), Box::new(body.clone())),
                )?;
                unify(mod_file, range, &actual_ty, &expect_ty)?
            }
            Top::DefineVar(range, name, expr) => {
                let expect_ty = match ctx.get(name) {
                    Some(ty) => ty.clone(),
                    None => Err(IdMissing {
                        src: mod_file.as_src(),
                        name: name.clone(),
                        span: range.to_span(),
                    })?,
                };
                let actual_ty = infer(mod_file, &ctx, expr)?;
                unify(mod_file, range, &actual_ty, &expect_ty)?
            }
            _ => unreachable!(),
        }
    }
    Ok(())
}

fn infer(mod_file: &ModFile, ctx: &Context, expr: &Expr) -> Result<Type, TyckError> {
    match expr {
        Expr::Id(range, n) => match ctx.get(n) {
            Some(ty) => Ok(ty.clone()),
            None => Err(IdMissing {
                src: mod_file.as_src(),
                name: n.clone(),
                span: range.to_span(),
            })?,
        },
        Expr::Int(_, _) => Ok(Type::Base("i64".to_string())),
        Expr::Lambda(_, p_list, body) => {
            let fresh_list: Vec<Type> = p_list.into_iter().map(|_| Type::Free(0)).collect();
            let mut lam_ctx = Context::new();
            lam_ctx.extend(ctx.clone());
            for (p, f) in multizip((p_list, &fresh_list)) {
                lam_ctx.insert(p.clone(), f.clone());
            }
            let result_ty = infer(mod_file, &lam_ctx, body)?;
            Ok(Type::Arrow(fresh_list, Box::new(result_ty.clone())))
        }
    }
}

#[test]
fn base_type() {
    unify(
        &ModFile::new(),
        &Range::R(0, 0),
        &Type::Base("A".to_string()),
        &Type::Base("A".to_string()),
    )
    .unwrap()
}
