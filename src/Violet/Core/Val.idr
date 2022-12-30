module Violet.Core.Val

import Data.Either
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
    | VSum Name (List (Name, List VTy))
    | VConstructor Name (List Val)

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
extendEnv : Env -> Name -> Val -> Env
extendEnv env x v = (x, v) :: env

export
eval : Env -> Tm -> Either EvalError Val
eval env tm = case tm of
  SrcPos tm => eval env tm.val
  Var x => case lookup x env of
    Just a => pure a
    _ => Left $ NoVar x
  Apply t u => case (!(eval env t), !(eval env u)) of
    (VLam _ t, u) => t u
    (VConstructor x vs, u) => pure $ VConstructor x $ vs ++ [u]
    (t, u) => pure $ VApp t u
  U => pure VU
  Lam x t => pure $ VLam x (\u => eval (extendEnv env x u) t)
  Pi x a b => pure $ VPi x !(eval env a) (\u => eval (extendEnv env x u) b)
  Let x a t u => eval (extendEnv env x !(eval env t)) u
  Postulate x a u => eval (extendEnv env x (VVar x)) u
  Elim t cases => ?todo1
  Sum x cases => pure $ VSum x $ map (\(name, ts) => (name, rights $ map (eval env) ts)) cases
  Intro x _ u => eval (extendEnv env x (VConstructor x [])) u

export
fresh : Env -> Name -> Name
fresh _ "_" = "_"
fresh env x = case lookup x env of
  Just _ => fresh env (x ++ "'")
  _ => x

export
quote : Env -> Val -> Either EvalError Tm
quote env v = case v of
  VVar x => pure $ Var x
  VApp t u => pure $ Apply !(quote env t) !(quote env u)
  VLam x t => do
    let x = fresh env x
    let vx = (VVar x)
    pure $ Lam x !(quote (extendEnv env x vx) !(t vx))
  VPi x a b => do
    let x = fresh env x
    pure $ Pi x !(quote env a) !(quote (extendEnv env x (VVar x)) !(b (VVar x)))
  VU => pure U
  VSum x cases => pure $ Var x
  VConstructor x vs => pure $ foldl Apply (Var x) $ rights $ map (quote env) vs

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
