module Violet.Syntax

import public Lightyear.Position

public export
Name : Type
Name = String

mutual
  ||| The Core Term of violet language
  public export
  data Tm
    = SrcPos Position Tm
    | Var Name             -- x
    | Lam Name Tm          -- λ x . t
    | App Tm Tm            -- t u
    | U                    -- U
    | Pi Name Ty Ty        -- Π (x : a) → b
    | Let Name Ty Tm Tm    -- let x : a = t; u
    -- FIXME?: maybe constructor should be treated special
    | Postulate Name Ty Tm -- posulate x : a; u

  public export
  Ty : Type
  Ty = Tm
