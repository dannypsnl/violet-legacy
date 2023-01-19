module Violet.Core.Val

import Data.Either
import Data.List
import Violet.Core.Term
import Violet.Error.Eval

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
