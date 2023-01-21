module Violet.Core.Val

import Data.Either
import Data.SortedMap
import Data.List
import Violet.Core.Term
import Violet.Core.Common
import Violet.Error.Eval

mutual
	public export
	data MetaEntry = Solved Val | Unsolved

	public export
	data Val
		= VVar Name
		| VApp Val Val
		| VLam Name (GlobalEnv -> Val -> Either EvalError Val)
		| VPi Mode Name VTy (Val -> Either EvalError Val)
		| VU
		-- data type
		| VData Name
		-- constructor
		| VCtor Name Spine
		-- meta variable
		| VMeta MetaVar

	public export
	VTy : Type
	VTy = Val

	public export
	GlobalEnv : Type
	GlobalEnv = List (Name, Val)

	public export
	Spine : Type
	Spine = List Val

public export
LocalEnv : Type
LocalEnv = List (Name, Val)

export
record MetaCtx where
	constructor MkMetaCtx
	map : SortedMap MetaVar MetaEntry
	counter : MetaVar

export
emptyMetaCtx : MetaCtx
emptyMetaCtx = MkMetaCtx empty 0

export
newMeta : MetaCtx -> (MetaVar, MetaCtx)
newMeta ctx = do
	let curCount = ctx.counter
	let newCtx = {counter $= S, map := insert curCount Unsolved ctx.map } ctx
	(curCount, newCtx)

export
solveMeta : MetaCtx -> MetaVar -> Val -> MetaCtx
solveMeta ctx var val = { map := insert var (Solved val) ctx.map } ctx

export
lookupMeta : MetaCtx -> MetaVar -> Either EvalError MetaEntry
lookupMeta ctx var = case lookup var ctx.map of
	Just entry => pure entry
	Nothing => Left $ NoMeta var

public export
record Env where
	constructor MkEnv
	global : GlobalEnv
	local : LocalEnv

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
