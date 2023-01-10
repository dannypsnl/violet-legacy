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
		| VLam Name Closure
		| VPi Name VTy Closure
		| VU

	public export
	VTy : Type
	VTy = Val

	record Closure where
		constructor MkClos
		name : Name
		env : LocalEnv
		tm : Tm

	public export
	LocalEnv : Type
	LocalEnv = List (Name, Val)

public export
GlobalEnv : Type
GlobalEnv = List (Name, Val)

public export
record Env where
	constructor MkEnv
	globalEnv : GlobalEnv
	localEnv : List (Name, Val)

export
extendLocalEnv : Env -> Name -> Val -> Env
extendLocalEnv env x v = { localEnv := (x, v) :: env.localEnv } env

export
lookupEnv : Name -> Env -> Maybe Val
lookupEnv x env = case lookup x env.localEnv of
	Just v => Just v
	_ => lookup x env.globalEnv

export
fresh : Env -> Name -> Name
fresh _ "_" = "_"
fresh env x = case lookupEnv x env of
	Just _ => fresh env (x ++ "'")
	_ => x

mutual
	export
	toSpine : Env -> Val -> Either EvalError $ List Val
	toSpine env (VVar x) = pure [VVar x]
	toSpine env (VApp t u) = pure (!(toSpine env t) ++ !(toSpine env u))
	toSpine env v = Left $ BadSpine !(quote env v)

	export
	quote : Env -> Val -> Either EvalError Tm
	quote env v = case v of
		VVar x => pure $ Var x
		VApp t u => pure $ Apply !(quote env t) !(quote env u)
		VLam x t => do
			let x = fresh env x
			let vx = (VVar x)
			pure $ Lam x !(quote (extendLocalEnv env x vx) !(eApp env.globalEnv t vx))
		VPi x a b => do
			let x = fresh env x
			pure $ Pi x !(quote env a) !(quote (extendLocalEnv env x (VVar x)) !(eApp env.globalEnv b (VVar x)))
		VU => pure U

	export
	eApp : GlobalEnv -> Closure -> Val -> Either EvalError Val
	eApp env (MkClos x localEnv t) u = eval (MkEnv env ((x, u) :: localEnv)) t

	export
	eval : Env -> Tm -> Either EvalError Val
	eval env tm = case tm of
		SrcPos tm => eval env tm.val
		Var x => case lookupEnv x env of
			Just a => pure a
			Nothing => Left $ NoVar x
		Apply t u => case (!(eval env t), !(eval env u)) of
			(VLam _ t, u) => eApp env.globalEnv t u
			(t, u) => pure $ VApp t u
		U => pure VU
		Lam x t => pure $ VLam x (MkClos x env.localEnv t)
		Pi x a b => pure $ VPi x !(eval env a) (MkClos x env.localEnv b)
		Let x a t u => eval (extendLocalEnv env x !(eval env t)) u
		Elim t cases => do
			spine <- toSpine env !(eval env t)
			go spine cases
			where
				matches : Pat -> List Val -> (Bool, LocalEnv)
				matches (PVar x) [VVar x'] = (x == x', [])
				matches (PCons h rest) (VVar h' :: rest') = (h == h', zip rest rest')
				matches _ _ = (False, [])

				go : List Val -> List (Pat, Tm) -> Either EvalError Val
				go spine ((pat, rhs) :: rest) = case matches pat spine of
					(True, env') => eval ({ localEnv := env' ++ env.localEnv } env) rhs
					(False, _) => go spine rest
				go spine [] = Left OutOfCase

export
nf : Env -> Tm -> Either EvalError Tm
nf env tm = quote env !(eval env tm)

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
extendCtxWithBinds : Ctx -> List (Name, VTy) -> Ctx
extendCtxWithBinds ctx binds = { map := binds ++ ctx.map } ctx

export
lookupCtx : Ctx -> Name -> Maybe VTy
lookupCtx ctx x = lookup x ctx.map
