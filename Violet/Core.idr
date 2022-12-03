module Violet.Core

import Violet.Syntax

mutual
  export
  data Val
    = VVar Name
    | VApp Val Val
    | VLam Name (Val -> Val)
    | VPi Name VTy (Val -> Val)
    | VU

  export
  VTy : Type
  VTy = Val

Env : Type
Env = List (Name, Val)
Ctx : Type
Ctx = List (Name, VTy)

-- eval : Env -> Tm -> Val
-- eval env tm = case tm of
--   Var x => ?a
--   App t u => case (eval env t, eval env u) of
--     (VLam _ t, u) => t u
--     (t, u) => VApp t u
--   _ => ?undefined