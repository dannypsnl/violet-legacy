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

partial export
Show Tm where
  show (SrcPos t) = show t.val
  show (Var name) = name
  show (Lam x t) = "Lam " ++ x ++ " " ++ show t
  show (App t u) = "App " ++ show t ++ " " ++ show u
  show U = "U"
  show (Pi x a b) = "Pi " ++ x ++ " " ++ show a ++ " " ++ show b
  show (Let x a t u) = "Let " ++ x ++ " " ++ show a ++ " " ++ show t ++ " " ++ show u
  show (Postulate x a u) = "Postulate " ++ x ++ " " ++ show a ++ " " ++ show u

export
Pretty Tm where
  pretty (SrcPos t) = pretty t.val
  pretty (Var name) = pretty name
  pretty (Lam x t) = hsep ["λ", pretty x, "=>", pretty t]
  pretty (App t u) = pretty t <++> case u of
    SrcPos t => parens $ pretty t.val
    App {}   => parens $ pretty u
    _        => pretty u
  pretty U = "U"
  pretty (Pi x a b) =
    hsep [ hcat ["(", pretty x],
           ":",
           hcat [pretty a, ")"],
           "→", pretty b
         ]
  pretty (Let x a t u) =
    hsep [ "let", pretty x, ":", pretty a, "=", hcat [pretty t, ";"] ]
    <++> line
    <++> pretty u
  pretty (Postulate x a u) =
    hsep ["postulate", pretty x, ":", hcat [pretty a, ";"]]
    <++> line
    <++> pretty u
