module Violet.Core.Term

public export
data Position = MkPos Int Int

export
Show Position where
  show (MkPos line col) = show line ++ ":" ++ show col ++ ":"

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
    | Pi Name Ty Ty        -- (x : a) → b
    | Let Name Ty Tm Tm    -- let x : a = t; u
    -- FIXME?: maybe constructor should be treated special
    | Postulate Name Ty Tm -- posulate x : a; u

  public export
  Ty : Type
  Ty = Tm

export
Show Tm where
  show (SrcPos _ t)        = show t
  show (Var name)        = name
  show (Lam x t)         = "λ " ++ x ++ "." ++ show t
  show (App t u)         = show t ++ " " ++ show u
  show U                 = "U"
  show (Pi x a b)        = "(" ++ x ++ " : " ++ show a ++ ") → " ++ show b
  show (Let x a t u)     = "let " ++ x ++ " : " ++ show a ++ " = " ++ show t ++ ";\n" ++ show u
  show (Postulate x a u) = "postulate " ++ x ++ " : " ++ show a ++ ";\n" ++ show u
