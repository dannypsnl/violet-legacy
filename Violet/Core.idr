module Violet.Core

import public Lightyear
import public Lightyear.Position

import Violet.Syntax
import public Violet.Val

export
data CheckError = MkCheckError (Maybe Position) String
export
Show CheckError where
  show (MkCheckError (Just pos) msg) = concat [ display pos, "\n", msg]
  show (MkCheckError _ msg) = msg

public export
checkM : Type -> Type
checkM a = Either CheckError a

addPos : Position -> checkM a -> checkM a
addPos pos (Left (MkCheckError Nothing msg)) = (Left (MkCheckError (Just pos) msg))
addPos _ ma = ma
report : String -> Either CheckError a
report msg = Left (MkCheckError Nothing msg)

eval : Env -> Tm -> Val
eval env tm = case tm of
  SrcPos _ tm => eval env tm
  Var x => case lookup x env of
    Just a => a
    _ => ?unreachable
  App t u => case (eval env t, eval env u) of
    (VLam _ t, u) => t u
    (t, u) => VApp t u
  U => VU
  Lam x t => VLam x (\u => eval (extend env x u) t)
  Pi x a b => VPi x (eval env a) (\u => eval (extend env x u) b)
  Let x a t u => eval (extend env x (eval env t)) u
  Postulate x a u => eval (extend env x (VVar x)) u

export
quote : Env -> Val -> Tm
quote env v = case v of
  VVar x => Var x
  VApp t u => App (quote env t) (quote env u)
  VLam x t =>
    let x = fresh env x
    in Lam x (quote (extend env x (VVar x)) (t (VVar x)))
  VPi x a b =>
    let x = fresh env x
    in Pi x (quote env a) (quote (extend env x (VVar x)) (b (VVar x)))
  VU => U

nf : Env -> Tm -> Tm
nf env tm = quote env (eval env tm)

nf0 : Tm -> Tm
nf0 = nf emptyEnv

mutual
  export
  infer : Env -> Ctx -> Tm -> checkM VTy
  infer env ctx tm = case tm of
    SrcPos pos t => addPos pos (infer env ctx t)
    Var x => case lookup x ctx of
      Nothing => report $ "variable: " ++ x ++ " not found"
      Just a => pure a
    U => pure VU
    App t u => do
      tty <- infer env ctx t
      case tty of
        VPi _ a b => do
          check env ctx u a
          pure (b (eval env u))
        _ => report "bad app"
    Lam _ _ => report $ "cannot inference lambda: " ++ show tm
    Pi x a b => do
      check env ctx a VU
      check (extend env x (VVar x)) (extendCtx ctx x (eval env a)) b VU
      pure VU
    Postulate x a u => do
      check env ctx a VU
      let a' = eval env a
      infer (extend env x (VVar x)) (extendCtx ctx x a') u
    Let x a t u => do
      check env ctx a VU
      let a' = eval env a
      check env ctx t a'
      infer (extend env x (eval env t)) (extendCtx ctx x a') u

  check : Env -> Ctx -> Tm -> VTy -> checkM ()
  check env ctx t a = case (t, a) of
    (SrcPos pos t, a) => addPos pos (check env ctx t a)
    (Lam x t, VPi x' a b) =>
      let x' = fresh env x'
      in check (extend env x (VVar x)) (extendCtx ctx x a) t (b (VVar x'))
    (Let x a t u, _) => do
      check env ctx a VU
      let a' = eval env a
      check env ctx t a'
      check (extend env x (eval env t)) (extendCtx ctx x a') u a'
    _ => do
      tty <- infer env ctx t
      if (conv env tty a)
        then pure ()
        else report $ unlines
          [ "type mismatched"
          , "expected type:\n"
          , "  " ++ (show $ quote env a)
          , "\nactual type:\n"
          , "  " ++ (show $ quote env tty)
          ]
  
  conv : Env -> Val -> Val -> Bool
  conv env t u = case (t, u) of
    (VU, VU) => True
    (VPi x a b, VPi _ a' b') =>
      let x = fresh env x
      in conv env a a' && conv (extend env x (VVar x)) (b (VVar x)) (b' (VVar x))
    (VLam x t, VLam _ t') =>
      let x = fresh env x
      in conv (extend env x (VVar x)) (t (VVar x)) (t' (VVar x))
    -- checking eta conversion for Lam
    (VLam x t, u) =>
      let x = fresh env x
      in conv (extend env x (VVar x)) (t (VVar x)) (VApp u (VVar x))
    (u, VLam x t) =>
      let x = fresh env x
      in conv (extend env x (VVar x)) (VApp u (VVar x)) (t (VVar x))
    (VVar x, VVar x') => x == x'
    (VApp t u, VApp t' u') => conv env t t' && conv env u u'
    _ => False
