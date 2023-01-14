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
		| VLam Name (LocalEnv -> Val -> Either EvalError Val)
		| VPi Name VTy (Val -> Either EvalError Val)
		| VU
		-- data type
		| VData Name
		-- constructor
		| VCtor Name Spine

	public export
	VTy : Type
	VTy = Val

	public export
	LocalEnv : Type
	LocalEnv = List (Name, Val)

	public export
	Spine : Type
	Spine = List Val

public export
GlobalEnv : Type
GlobalEnv = List (Name, Val)

public export
record Env where
	constructor MkEnv
	global : GlobalEnv
	local : LocalEnv

export
emptyEnv : Env
emptyEnv = MkEnv [] []

export
extendEnv : Env -> Name -> Val -> Env
extendEnv env x v = { local := (x, v) :: env.local } env

export
lookupEnv : Name -> Env -> Either EvalError Val
lookupEnv x env = do
	Just v <- pure $ lookup x env.local <|> lookup x env.global
		| Nothing => Left $ NoVar x
	pure v

export
fresh : Env -> Name -> Name
fresh _ "_" = "_"
fresh env x = case lookupEnv x env of
	Right _ => fresh env (x ++ "'")
	_ => x

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

app : GlobalEnv -> Val -> Val -> Either EvalError Val
app env t' u with (t')
	_ | (VLam _ t) = t env u
	_ | (VCtor x spine) = pure $ VCtor x (spine ++ [u])
	_ | t = pure $ VApp t u

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
			matches : List Pat -> List Val -> Either EvalError LocalEnv
			matches (PCons c [] :: next) (VCtor c' vals :: next') =
				if not (c == c')
					then pure $ (c, (VCtor c' vals)) :: !(matches next next')
					else if 0 == length vals
						then matches next next'
						else Left OutOfCase
			matches (PCons c names :: next) (VCtor c' vals :: next') =
				if c == c' && length names == length vals
					then pure $ !(matches next next') ++ (names `zip` vals)
					else Left OutOfCase
			matches [] [] = pure []
			matches _ _  = Left OutOfCase

			go : List Val -> List (List Pat, Tm) -> Either EvalError Val
			go vals ((pats, rhs) :: nextPat) = case matches pats vals of
				Right lenv => eval ({ local := lenv ++ env.local } env) rhs
				Left _ => go vals nextPat
			go vals [] = Left OutOfCase

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
