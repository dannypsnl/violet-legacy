module Violet.Core.Eval

import Data.List
import public Violet.Core.Val
import Violet.Core.Term
import public Violet.Error.Eval

app : GlobalEnv -> Val -> Val -> Either EvalError Val
app env t' u with (t')
  _ | (VLam _ t) = t env u
  _ | (VCtor x spine) = pure $ VCtor x (spine ++ [u])
  _ | t = pure $ VApp t u

vMeta : MetaCtx -> MetaVar -> Either EvalError Val
vMeta ctx m = case !(lookupMeta ctx m) of
  Solved v => pure v
  Unsolved => pure $ VMeta m

mutual
  export
  eval : MetaCtx -> Env -> Tm -> Either EvalError Val
  eval mctx env tm = case tm of
    Var x => lookupEnv x env
    Apply t u => app env.global !(eval mctx env t) !(eval mctx env u)
    U => pure VU
    Lam x t => pure $ VLam x (\global, u => eval mctx (extendEnv (MkEnv global env.local) x u) t)
    Pi mode x a b => pure $ VPi mode x !(eval mctx env a) (\u => eval mctx (extendEnv env x u) b)
    Let x a t u => eval mctx (extendEnv env x !(eval mctx env t)) u
    Meta m => vMeta mctx m
    Elim ts cases => go !(for ts (eval mctx env)) cases
    where
      go : List Val -> List ElimCase -> Either EvalError Val
      go vals (ECase pats rhs :: nextPat) = case matches pats vals of
        Just lenv => eval mctx ({ local := lenv ++ env.local } env) rhs
        Nothing => go vals nextPat
      go vals [] = Left OutOfCase

  matches : List Pat -> List Val -> Maybe LocalEnv
  -- var pattern is from elaboration
  -- e.g. with context `x : Nat`
  --
  --   elim x
  --   | m => ...
  matches (PVar x :: next) (val :: next') = pure $ (x, val) :: !(matches next next')
  -- ctor pattern after elaboration must be a correct ctor pattern
  -- e.g. with context `x : Nat`
  --
  --   elim x
  --   | zero => ...
  --   | suc _ => ...
  matches (PCtor c names :: next) (VCtor c' vals :: next') =
    if c == c' && length names == length vals
      then pure $ (names `zip` vals) ++ !(matches next next')
      else Nothing
  matches [] [] = pure []
  matches _ _  = Nothing

mutual
  quoteMeta : MetaCtx -> Env -> MetaVar -> Either EvalError Tm
  quoteMeta ctx env m = case !(lookupMeta ctx m) of
    Solved v => quote ctx env v
    Unsolved => pure $ Meta m

  export
  quote : MetaCtx -> Env -> Val -> Either EvalError Tm
  quote mctx env v = case v of
    VVar x => pure $ Var x
    VApp t u => pure $ Apply !(quote mctx env t) !(quote mctx env u)
    VLam x t => do
      let x = fresh env x
      let vx = (VVar x)
      pure $ Lam x !(quote mctx (extendEnv env x vx) !(t env.global vx))
    VPi mode x a b => do
      let x = fresh env x
      pure $ Pi mode x !(quote mctx env a) !(quote mctx (extendEnv env x (VVar x)) !(b (VVar x)))
    VU => pure U
    VData x => pure $ Var x
    VCtor x spine => pure $ foldl (\acc, s => acc `Apply` s) (Var x) !(for spine (quote mctx env))
    VMeta m => quoteMeta mctx env m

export
nf : MetaCtx -> Env -> Tm -> Either EvalError Tm
nf mctx env tm = quote mctx env !(eval mctx env tm)
