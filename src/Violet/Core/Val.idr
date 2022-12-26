module Violet.Core.Val

import Data.List
import Violet.Core.Term

mutual
  public export
  data Val
    = VVar Name
    | VApp Val Val
    | VLam Name (Val -> Val)
    | VPi Name VTy (Val -> Val)
    | VU

  public export
  VTy : Type
  VTy = Val

public export
Env : Type
Env = List (Name, Val)
export
emptyEnv : Env
emptyEnv = []

export
fresh : Env -> Name -> Name
fresh _ "_" = "_"
fresh env x = case lookup x env of
  Just _ => fresh env (x ++ "'")
  _ => x

export
extend : Env -> Name -> Val -> Env
extend env x v = (x, v) :: env

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
lookupCtx : Ctx -> Name -> Maybe VTy
lookupCtx ctx x = lookup x ctx.map
