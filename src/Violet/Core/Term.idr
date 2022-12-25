module Violet.Core.Term

import Data.String
import Text.Parser.Core
import Text.PrettyPrint.Prettyprinter.Doc
import Text.PrettyPrint.Prettyprinter.Symbols
import Text.PrettyPrint.Prettyprinter.Render.Terminal

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

prettyTm : Tm -> Doc ann
prettyTm (SrcPos t)        = prettyTm (t.val)
prettyTm (Var name)        = pretty name
prettyTm (Lam x t)         = hsep ["λ", pretty x, "=>", prettyTm t]
-- FIXME: should wrap nested app but leave rest
prettyTm (App t u)         = hsep [prettyTm t, prettyTm u]
prettyTm U                 = "U"
prettyTm (Pi x a b)        =
  hsep [ hcat ["(", pretty x],
         ":",
         hcat [prettyTm a, ")"],
         "→", prettyTm b
       ]
prettyTm (Let x a t u)     =
  hsep [ "let", pretty x, ":", prettyTm a, "=", hcat [prettyTm t, ";"] ]
  <++> line
  <++> prettyTm u
prettyTm (Postulate x a u) =
  hsep ["postulate", pretty x, ":", hcat [prettyTm a, ";"]]
  <++> line
  <++> prettyTm u
export
Pretty Tm where
  pretty = prettyTm
