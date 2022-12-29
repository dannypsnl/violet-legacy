module Violet.Core

import System
import Control.App
import Control.App.Console
import Data.List
import Data.String
import Text.Bounded
import Violet.Core.Term
import public Violet.Core.Val
import public Violet.Error.Check

report : Has [Exception CheckError] e => Ctx -> CheckErrorKind -> App e a
report ctx err = do
    throw $ MkCheckError ctx.filename ctx.source Nothing err

addPos : Has [Exception CheckError] e => Bounds -> App e a -> App e a
addPos bounds app = catch app
    (\ce => case ce.bounds of
      Nothing => let err : CheckError = {bounds := Just bounds} ce in throw err
      Just _ => throw ce)

cast : EvalError -> CheckErrorKind
cast (NoVar x) = NoVar x

export
runEval : Has [Exception CheckError] e => (Env -> a -> Either EvalError b) -> Ctx -> Env -> a -> App e b
runEval f ctx env a = case f env a of
  Left e => report ctx $ cast e
  Right b => pure b

emptyEnvAndCtx : (Env, Ctx)
emptyEnvAndCtx = (emptyEnv, emptyCtx)

mutual
  infer : Has [Exception CheckError] e => Env -> Ctx -> Tm -> App e VTy
  infer env ctx tm = do
    (ty, _) <- infer' env ctx tm
    pure ty

  -- infer but with new introduced env and ctx (only top level)
  export
  infer' : Has [Exception CheckError] e => Env -> Ctx -> Tm -> App e (VTy, (Env, Ctx))
  infer' env ctx tm = go tm
    where
      go : Tm -> App e (VTy, (Env, Ctx))
      go (SrcPos t) = addPos t.bounds (go t.val)
      go (Var x) = case lookupCtx ctx x of
        Nothing => report ctx (NoVar x)
        Just a => pure (a, emptyEnvAndCtx)
      go U = pure (VU, emptyEnvAndCtx)
      go (Apply t u) = do
        VPi _ a b <- infer env ctx t
          | t' => report ctx $ BadApp !(runEval quote ctx env t')
        check env ctx u a
        u' <- runEval eval ctx env u
        case b u' of
          Right b' => pure (b', emptyEnvAndCtx)
          Left e => report ctx $ cast e
      go (Lam {}) = report ctx (InferLam tm)
      go (Pi x a b) = do
        check env ctx a VU
        let newEnv = extend emptyEnv x (VVar x)
            newCtx = extendCtx emptyCtx x !(runEval eval ctx env a)
        check (newEnv <+> env) (newCtx <+> ctx) b VU
        pure (VU, (newEnv, newCtx))
      go (Postulate x a u) = do
        check env ctx a VU
        let newEnv = extend emptyEnv x (VVar x)
            newCtx = extendCtx emptyCtx x !(runEval eval ctx env a)
        (ty, restEnvAndCtx) <- infer' (newEnv <+> env) (newCtx <+> ctx) u
        pure (ty, (newEnv, newCtx) <+> restEnvAndCtx)
      go (Let x a t u) = do
        check env ctx a VU
        a' <- runEval eval ctx env a
        check env ctx t a'
        let newEnv = extend emptyEnv x !(runEval eval ctx env t)
            newCtx = extendCtx emptyCtx x a'
        (ty, restEnvAndCtx) <- infer' (newEnv <+> env) (newCtx <+> ctx) u
        pure (ty, (newEnv, newCtx) <+> restEnvAndCtx)
      go (Elim t cases) = ?todo2

  check : Has [Exception CheckError] e => Env -> Ctx -> Tm -> VTy -> App e ()
  check env ctx t a = go t a
    where
      go : Tm -> VTy -> App e ()
      go (SrcPos t) a = addPos t.bounds (go t.val a)
      go (Lam x t) (VPi x' a b) = do
        let x' = fresh env x'
        case b (VVar x') of
          Right u => check (extend env x (VVar x)) (extendCtx ctx x a) t u
          Left err => report ctx $ cast err
      go (Let x a t u) _ = do
        check env ctx a VU
        a' <- runEval eval ctx env a
        check env ctx t a'
        check (extend env x !(runEval eval ctx env t)) (extendCtx ctx x a') u a'
      go _ _ = do
        tty <- infer env ctx t
        case conv env tty a of
          Right convertable =>
            if convertable
              then pure ()
              else report ctx $ TypeMismatch !(runEval quote ctx env a) !(runEval quote ctx env tty)
          Left err => report ctx $ cast err

  conv : Env -> Val -> Val -> Either EvalError Bool
  conv env t u = go t u
    where
      go : Val -> Val -> Either EvalError Bool
      go VU VU = pure True
      go (VPi x a b) (VPi _ a' b') = do
        let x' = fresh env x
        pure $ !(conv env a a') && !(conv (extend env x' (VVar x')) !(b (VVar x')) !(b' (VVar x')))
      go (VLam x t) (VLam _ t') = do
        let x = fresh env x
        conv (extend env x (VVar x)) !(t (VVar x)) !(t' (VVar x))
      -- checking eta conversion for Lam
      go (VLam x t) u = do
        let x = fresh env x
        conv (extend env x (VVar x)) !(t (VVar x)) (VApp u (VVar x))
      go u (VLam x t) = do
        let x = fresh env x
        conv (extend env x (VVar x)) (VApp u (VVar x)) !(t (VVar x))
      go (VVar x) (VVar x') = pure $ x == x'
      go (VApp t u) (VApp t' u') = pure $ !(conv env t t') && !(conv env u u')
      go _ _ = pure False
