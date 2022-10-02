use crate::ast::*;
use miette::Result;
use std::collections::HashMap;
pub mod error;
use error::{IdMissing, TyMismatch, TyckError};

type Context = HashMap<String, Type>;

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
            Top::DefineProc(name, p_list, body) => {}
            Top::DefineVar(name, expr) => {
                let actual_ty = infer(&ctx, expr)?;
                let expect_ty = match ctx.get(name) {
                    Some(ty) => ty.clone(),
                    None => Err(IdMissing { name: name.clone() })?,
                };
                if actual_ty == expect_ty.clone() {
                } else {
                    return Err(TyMismatch {
                        expected: expect_ty.clone(),
                        actual: actual_ty,
                    })?;
                }
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
    }
}
