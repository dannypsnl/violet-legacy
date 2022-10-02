use crate::ast::*;
use miette::Result;
use std::{borrow::Borrow, collections::HashMap};
pub mod error;
use error::{IdMissing, TyMismatch, TyckError};

type Context = HashMap<String, Type>;

fn unify(t1: Type, t2: Type) -> Result<(), TyckError> {
    if t1 == t2 {
    } else {
        return Err(TyMismatch {
            expected: t2,
            actual: t1,
        })?;
    }
    Ok(())
}

pub fn check_module(top_list: &Vec<Top>) -> Result<(), TyckError> {
    let mut ctx = Context::new();
    let mut to_check_list: Vec<&Top> = vec![];
    for top in top_list {
        match top {
            Top::TypeDecl(name, ty) => {
                ctx.insert(name.clone(), ty.clone());
            }
            Top::DefineProc(_, _, _) => to_check_list.push(top),
            Top::DefineVar(_, _) => to_check_list.push(top),
        }
    }
    for to_c in to_check_list {
        match to_c {
            Top::DefineProc(name, p_list, body) => {
                let expect_ty = match ctx.get(name) {
                    Some(ty) => ty.clone(),
                    None => Err(IdMissing { name: name.clone() })?,
                };
                let actual_ty = infer(&ctx, &Expr::Lambda(p_list.clone(), Box::new(body.clone())))?;
                unify(actual_ty, expect_ty)?
            }
            Top::DefineVar(name, expr) => {
                let expect_ty = match ctx.get(name) {
                    Some(ty) => ty.clone(),
                    None => Err(IdMissing { name: name.clone() })?,
                };
                let actual_ty = infer(&ctx, expr)?;
                unify(actual_ty, expect_ty)?
            }
            _ => unreachable!(),
        }
    }
    Ok(())
}

fn infer(ctx: &Context, expr: &Expr) -> Result<Type, TyckError> {
    match expr {
        Expr::Id(n) => match ctx.get(n) {
            Some(ty) => Ok(ty.clone()),
            None => Err(IdMissing { name: n.clone() })?,
        },
        Expr::Int(_) => Ok(Type::Base("i64".to_string())),
        Expr::Lambda(p_list, body) => {
            let mut lam_ctx = Context::new();
            lam_ctx.extend(ctx.clone());
            for p in p_list {
                lam_ctx.insert(p.clone(), Type::Free());
            }
            let result_ty = infer(&lam_ctx, body)?;
            Ok(Type::Arrow(
                p_list.into_iter().map(|_| Type::Free()).collect(),
                Box::new(result_ty.clone()),
            ))
        }
    }
}
