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

mutual
	export
	eval : Env -> Tm -> Either EvalError Val
	eval env tm = case tm of
		SrcPos tm => eval env tm.val
		Var x => lookupEnv x env
		Apply t u => app env.global !(eval env t) !(eval env u)
		U => pure VU
		Lam x t => pure $ VLam x (\global, u => eval (extendEnv (MkEnv global env.local) x u) t)
		Pi x a b => pure $ VPi x !(eval env a) (\u => eval (extendEnv env x u) b)
		Let x a t u => eval (extendEnv env x !(eval env t)) u
		Elim ts cases => go !(for ts (eval env)) cases
		where
			go : List Val -> List (List Pat, Tm) -> Either EvalError Val
			go vals ((pats, rhs) :: nextPat) = case matches pats vals of
				Just lenv => eval ({ local := lenv ++ env.local } env) rhs
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

export
quote : Env -> Val -> Either EvalError Tm
quote env v = case v of
	VVar x => pure $ Var x
	VApp t u => pure $ Apply !(quote env t) !(quote env u)
	VLam x t => do
		let x = fresh env x
		let vx = (VVar x)
		pure $ Lam x !(quote (extendEnv env x vx) !(t env.global vx))
	VPi x a b => do
		let x = fresh env x
		pure $ Pi x !(quote env a) !(quote (extendEnv env x (VVar x)) !(b (VVar x)))
	VU => pure U
	VData x => pure $ Var x
	VCtor x spine => pure $ foldl (\acc, s => acc `Apply` s) (Var x) !(for spine (quote env))

export
nf : Env -> Tm -> Either EvalError Tm
nf env tm = quote env !(eval env tm)
