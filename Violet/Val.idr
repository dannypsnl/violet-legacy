module Violet.Val

import Violet.Syntax

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

public export
Ctx : Type
Ctx = List (Name, VTy)
export
emptyCtx : Ctx
emptyCtx = []

export
fresh : Env -> Name -> Name
fresh _ "_" = "_"
fresh env x = case lookup x env of
  Just _ => fresh env (x ++ "'")
  _ => x

export
extend : Env -> Name -> Val -> Env
extend env x v = (x, v) :: env
