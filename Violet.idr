module Violet

Name : Type
Name = String
data Tm
  = SrcPos Tm
  | Var Name             -- x
  | Lam Name Tm          -- λ x . t
  | App Tm Tm            -- t u
  | U                    -- U
  | Pi Name Tm Tm        -- Π (x : a) → b
  | Let Name Tm Tm Tm    -- let x : a = t; u
  -- FIXME?: maybe constructor should be treated special
  | Postulate Name Tm Tm -- posulate x : a; u
Ty : Type
Ty = Tm

data Val
  = VVar Name
  | VApp Val Val
  | VLam Name (Val -> Val)
  | VPi Name Val (Val -> Val)
  | VU
VTy : Type
VTy = Val

Env : Type
Env = List (Name, Val)
Ctx : Type
Ctx = List (Name, VTy)

eval : Env -> Tm -> Val
eval env tm = case tm of
  Var x => ?a
  App t u => case (eval env t, eval env u) of
    (VLam _ t, u) => t u
    (t, u) => VApp t u
  _ => ?undefined
