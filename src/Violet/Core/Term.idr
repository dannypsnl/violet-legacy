module Violet.Core.Term

import Text.Parser.Core
import public Violet.Core.Position

public export
Name : Type
Name = String

mutual
  ||| The Core Term of violet language
  public export
  data Tm
    = SrcPos (WithBounds Tm)
    | Var Name             -- x
    | Lam Name Tm          -- λ x => t
    | App Tm Tm            -- t u
    | U                    -- U
    | Pi Name Ty Ty        -- (x : a) → b
    | Let Name Ty Tm Tm    -- let x : a = t; u
    -- FIXME?: maybe constructor should be treated special
    | Postulate Name Ty Tm -- posulate x : a; u

  public export
  Ty : Type
  Ty = Tm

showTm : Tm -> String
showTm (SrcPos t)        = showTm (t.val)
showTm (Var name)        = name
showTm (Lam x t)         = "λ " ++ x ++ "." ++ showTm t
showTm (App t u)         = showTm t ++ " " ++ showTm u
showTm U                 = "U"
showTm (Pi x a b)        = "(" ++ x ++ " : " ++ showTm a ++ ") → " ++ showTm b
showTm (Let x a t u)     = "let " ++ x ++ " : " ++ showTm a ++ " = " ++ showTm t ++ ";\n" ++ showTm u
showTm (Postulate x a u) = "postulate " ++ x ++ " : " ++ showTm a ++ ";\n" ++ showTm u

export partial
Show Tm where
  show = showTm
