module Violet.Core.Val

import Data.List
import Violet.Core.Term
import public Violet.Error.Eval

mutual
  public export
  data Val
    = VVar Name
    | VApp Val Val
    | VLam Name (Val -> Either EvalError Val)
    | VPi Name VTy (Val -> Either EvalError Val)
    | VU

  public export
  VTy : Type
  VTy = Val

public export
Env : Type
Env = List (Name, Val)

export
emptyEnv : Env
emptyEnv = []

export
fresh : Env -> Name -> Name
fresh _ "_" = "_"
fresh env x = case lookup x env of
  Just _ => fresh env (x ++ "'")
  _ => x

export
extend : Env -> Name -> Val -> Env
extend env x v = (x, v) :: env

export
eval : Env -> Tm -> Either EvalError Val
eval env tm = case tm of
  SrcPos tm => eval env tm.val
  Var x => case lookup x env of
    Just a => pure a
    _ => Left $ NoVar x
  Apply t u => case (!(eval env t), !(eval env u)) of
    (VLam _ t, u) => t u
    (t, u) => pure $ VApp t u
  U => pure VU
  Lam x t => pure $ VLam x (\u => eval (extend env x u) t)
  Pi x a b => pure $ VPi x !(eval env a) (\u => eval (extend env x u) b)
  Let x a t u => eval (extend env x !(eval env t)) u
  Postulate x a u => eval (extend env x (VVar x)) u
  Elim t cases => ?todo1

export
quote : Env -> Val -> Either EvalError Tm
quote env v = case v of
  VVar x => pure $ Var x
  VApp t u => pure $ Apply !(quote env t) !(quote env u)
  VLam x t => do
    let x = fresh env x
    let vx = (VVar x)
    pure $ Lam x !(quote (extend env x vx) !(t vx))
  VPi x a b => do
    let x = fresh env x
    pure $ Pi x !(quote env a) !(quote (extend env x (VVar x)) !(b (VVar x)))
  VU => pure U

nf : Env -> Tm -> Either EvalError Tm
nf env tm = quote env !(eval env tm)

export
nf0 : Tm -> Either EvalError Tm
nf0 = nf emptyEnv

-- context
public export
record Ctx where
  constructor MkCtx
  filename, source : String
  map : List (Name, VTy)

export
Semigroup Ctx where
  a <+> b = MkCtx b.filename b.source (a.map ++ b.map)

export
ctxFromFile : String -> String -> Ctx
ctxFromFile filename source = MkCtx filename source []

export
emptyCtx : Ctx
emptyCtx = MkCtx "<no file>" "<empty>" []

export
extendCtx : Ctx -> Name -> VTy -> Ctx
extendCtx ctx x v = { map := (x, v) :: ctx.map } ctx
export
lookupCtx : Ctx -> Name -> Maybe VTy
lookupCtx ctx x = lookup x ctx.map
