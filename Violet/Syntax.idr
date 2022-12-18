module Violet.Syntax

import Data.String
import Violet.Core.Position
import public Violet.Core.Term

mutual
  ||| The Core Term of violet language
  public export
  data Raw
    = RSrcPos Position Raw
    | RVar Name               -- x
    | RLam Name Raw           -- λ x . t
    | RApp Raw Raw            -- t u
    | RU                      -- U
    | RPi Name RTy RTy        -- (x : a) → b
    | RLet Name RTy Raw Raw   -- let x : a = t; u
    | RPostulate Name RTy Raw -- posulate x : a; u
    -- inductive type
    -- data Nat
    -- | zero : Nat
    -- | suc : Nat -> Nat
    -- ; u
    | RData Name (List (Name, RTy)) Raw

  public export
  RTy : Type
  RTy = Raw

export
toTm : Raw -> Tm
toTm (RSrcPos pos raw) = SrcPos pos (toTm raw)
toTm (RVar x) = Var x
toTm (RLam x t) = Lam x (toTm t)
toTm (RApp t u) = App (toTm t) (toTm u)
toTm RU = U
toTm (RPi x a b) = Pi x (toTm a) (toTm b)
toTm (RLet x a t u) = Let x (toTm a) (toTm t) (toTm u)
toTm (RPostulate x a u) = Postulate x (toTm a) (toTm u)
toTm (RData x caseLst r) = foldl (\a, (x, t) => \u => a (Postulate x (toTm t) u))
  (\u => Postulate x U u)
  caseLst
  $ toTm r

partial
export
Show Raw where
  show (RSrcPos _ t)      = show t
  show (RVar name)        = name
  show (RLam x t)         = "λ " ++ x ++ "." ++ show t
  show (RApp t u)         = show t ++ " " ++ show u
  show RU                 = "U"
  show (RPi x a b)        = "(" ++ x ++ " : " ++ show a ++ ") → " ++ show b
  show (RLet x a t u)     = "let " ++ x ++ " : " ++ show a ++ " = " ++ show t ++ ";\n" ++ show u
  show (RPostulate x a u) = "postulate " ++ x ++ " : " ++ show a ++ ";\n" ++ show u
  show (RData x caseLst u)  = "data" ++ x ++ (unlines $ map showCase caseLst) ++ show u
    where
      showCase : (Name, RTy) -> String
      showCase (x, t) = "| " ++ x ++ " : " ++ show t
