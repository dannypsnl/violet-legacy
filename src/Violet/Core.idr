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

public export
record CheckState where
  constructor MkCheckState
  topCtx : Ctx
  topEnv : Env

export
interface Has [Exception CheckError, State CheckState CheckState] e => Check e where
  getState : App e CheckState
  putState : CheckState -> App e ()

  updateEnv : Name -> Val -> App e()
  updateCtx : Name -> VTy -> App e()

  report : CheckErrorKind -> App e a
  addPos : Bounds -> App e a -> App e a
export
Has [Exception CheckError, State CheckState CheckState] e => Check e where
  getState = get CheckState
  putState = put CheckState

  updateEnv x v = do
    state <- getState
    putState $ { topEnv := extendEnv state.topEnv x v } state
  updateCtx x vty = do
    state <- getState
    putState $ { topCtx := extendCtx state.topCtx x vty } state

  report err = do
    state <- get CheckState
    let ctx = state.topCtx
    throw $ MkCheckError ctx.filename ctx.source Nothing err
  addPos bounds app = catch app
    (\ce => case ce.bounds of
      Nothing => let err : CheckError = {bounds := Just bounds} ce in throw err
      Just _ => throw ce)

export
Cast EvalError CheckErrorKind where
  cast (NoVar x) = NoVar x

export
runEval : Check es => (e -> a -> Either EvalError b) -> e -> a -> App es b
runEval f env a = do
  Right b <- pure $ f env a
    | Left e => report $ cast e
  pure b

mutual
  export
  infer' : Check e => Tm -> App e (VTy, CheckState)
  infer' tm = do
    state <- getState
    vty <- infer state.topEnv state.topCtx tm
    pure (vty, !getState)

  infer : Check e => Env -> Ctx -> Tm -> App e VTy
  infer env ctx tm = go tm
    where
      go : Tm -> App e VTy
      go (SrcPos t) = addPos t.bounds (go t.val)
      go (Var x) = case lookupCtx ctx x of
        Nothing => report (NoVar x)
        Just a => pure a
      go U = pure VU
      go (Apply t u) = case !(infer env ctx t) of
        VPi _ a b => do
          check env ctx u a
          u' <- runEval eval env u
          Right b' <- pure $ b u'
            | Left e => report $ cast e
          pure b'
        VSum x cs => pure $ VSum x cs
        t' => report $ BadApp !(runEval quote env t')
      go (Lam {}) = report (InferLam tm)
      go (Pi x a b) = do
        check env ctx a VU
        check (extendEnv env x (VVar x)) (extendCtx ctx x !(runEval eval env a)) b VU
        pure VU
      go (Postulate x a u) = do
        check env ctx a VU
        let env' = extendEnv env x (VVar x)
            ctx' = extendCtx ctx x !(runEval eval env a)
        updateEnv x (VVar x)
        updateCtx x !(runEval eval env a)
        ty <- infer env' ctx' u
        pure ty
      go (Let x a t u) = do
        check env ctx a VU
        a' <- runEval eval env a
        check env ctx t a'
        t' <- runEval eval env t
        let env' = extendEnv env x t'
            ctx' = extendCtx ctx x a'
        updateEnv x t'
        updateCtx x a'
        ty <- infer env' ctx' u
        pure ty
      go (Elim t cases) = ?todo2
      go (Sum {}) = pure VU
      go (Intro x t u) = do
        let env' = extendEnv env x (VConstructor x [])
            ctx' = extendCtx ctx x !(runEval eval env t)
        updateEnv x (VConstructor x [])
        updateCtx x !(runEval eval env t)
        infer env' ctx' u

  check : Check e => Env -> Ctx -> Tm -> VTy -> App e ()
  check env ctx t a = go t a
    where
      go : Tm -> VTy -> App e ()
      go (SrcPos t) a = addPos t.bounds (go t.val a)
      go (Lam x t) (VPi x' a b) = do
        let x' = fresh env x'
        case b (VVar x') of
          Right u => check (extendEnv env x (VVar x)) (extendCtx ctx x a) t u
          Left err => report $ cast err
      go (Let x a t u) _ = do
        check env ctx a VU
        a' <- runEval eval env a
        check env ctx t a'
        let env' = (extendEnv env x !(runEval eval env t))
            ctx' = (extendCtx ctx x a')
        updateEnv x !(runEval eval env t)
        updateCtx x a'
        check env' ctx' u a'
      go t (VSum x cases) = case !(runEval eval env t) of
        VConstructor x vs => case lookup x cases of
          Just ts => for_ (zip vs ts) $
            \(v, t) => check env ctx !(runEval quote env v) t
          Nothing => report (NoConstructor x)
        t => report (NotExpectedType x !(runEval quote env t))
      go _ _ = do
        tty <- infer env ctx t
        Right convertable <- pure $ conv env tty a
          | Left err => report $ cast err
        if convertable
          then pure ()
          else report $ TypeMismatch !(runEval quote env a) !(runEval quote env tty)

  conv : Env -> Val -> Val -> Either EvalError Bool
  conv env t u = go t u
    where
      go : Val -> Val -> Either EvalError Bool
      go VU VU = pure True
      go (VPi x a b) (VPi _ a' b') = do
        let x' = fresh env x
        pure $ !(conv env a a') && !(conv (extendEnv env x' (VVar x')) !(b (VVar x')) !(b' (VVar x')))
      go (VLam x t) (VLam _ t') = do
        let x = fresh env x
        conv (extendEnv env x (VVar x)) !(t (VVar x)) !(t' (VVar x))
      -- checking eta conversion for Lam
      go (VLam x t) u = do
        let x = fresh env x
        conv (extendEnv env x (VVar x)) !(t (VVar x)) (VApp u (VVar x))
      go u (VLam x t) = do
        let x = fresh env x
        conv (extendEnv env x (VVar x)) (VApp u (VVar x)) !(t (VVar x))
      go (VVar x) (VVar x') = pure $ x == x'
      go (VApp t u) (VApp t' u') = pure $ !(conv env t t') && !(conv env u u')
      go _ _ = pure False
