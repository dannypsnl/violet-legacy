module Violet.Core

import public Lightyear

import Violet.Syntax
import public Violet.Val

export
Env : Type
Env = List (Name, Val)
export
emptyEnv : Env
emptyEnv = []

export
Ctx : Type
Ctx = List (Name, VTy)
export
emptyCtx : Ctx
emptyCtx = []

export
data CheckError = MkCheckError (Maybe Position) String
export
Show CheckError where
  show (MkCheckError _pos msg) = msg

addPos : Position -> Either CheckError a -> Either CheckError a
addPos pos (Left (MkCheckError Nothing msg)) = (Left (MkCheckError (Just pos) msg))
addPos _ ma = ma
report : String -> Either CheckError a
report msg = Left (MkCheckError Nothing msg)

extend : Env -> Name -> Val -> Env
extend env x v = (x, v) :: env

eval : Env -> Tm -> Val
eval env tm = case tm of
  SrcPos pos tm => eval env tm
  Var x => ?a
  App t u => case (eval env t, eval env u) of
    (VLam _ t, u) => t u
    (t, u) => VApp t u
  U => VU
  Lam x t => VLam x $ \u => eval (extend env x u) t
  Pi x a b => VPi x (eval env a) $ \u => eval (extend env x u) b
  Let x a t u => eval (extend env x (eval env t)) u
  Postulate x a u => eval (extend env x (VVar x)) u

fresh : Env -> Name -> Name
fresh env n = n

mutual
  export
  infer : Env -> Ctx -> Tm -> Either CheckError VTy
  infer env ctx tm = case tm of
    SrcPos pos t => addPos pos (infer env ctx t)
    Var x => case lookup x ctx of
      Nothing => report "variable not found"
      Just a => pure a
    U => pure VU
    App t u => do
      tty <- infer env ctx t
      case tty of
        VPi _ a b => do
          check env ctx u a
          pure (b (eval env u))
        _ => report "bad app"
    Lam => report "cannot inference lambda"
    Pi x a b => do
      check env ctx a VU
      check (extend env x $ VVar x) ((x, eval env a) :: ctx) b VU
      pure VU
    Postulate x a u => do
      check env ctx a VU
      let a' = eval env a
      infer (extend env x $ VVar x) ((x, a') :: ctx) u
    Let x a t u => do
      check env ctx a VU
      let a' = eval env a
      check env ctx t a'
      infer (extend env x $ eval env t) ((x, a') :: ctx) u

  check : Env -> Ctx -> Tm -> VTy -> Either CheckError ()
  check env ctx t a = case (t, a) of
    (SrcPos pos t, a) => addPos pos (check env ctx t a)
    (Lam x t, VPi x' a b) =>
      let x' = fresh env x'
      in check (extend env x $ VVar x) ((x, a) :: ctx) t (b (VVar x'))
    (Let x a t u, _) => do
      check env ctx a VU
      let a' = eval env a
      check env ctx t a'
      check (extend env x $ eval env t) ((x, a') :: ctx) u a'
    _ => do
      tty <- infer env ctx t
      if (conv env tty a)
        then pure ()
        else report "type mismatched"
  
  conv : Env -> Val -> Val -> Bool
  conv env _ _ = True
