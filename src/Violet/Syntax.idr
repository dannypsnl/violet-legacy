module Violet.Syntax

import Data.String
import Text.Parser.Core
import public Violet.Core.Term

mutual
  ||| The Core Term of violet language
  public export
  data Raw
    = RSrcPos (WithBounds Raw)
    | RVar Name               -- x
    | RLam Name Raw           -- λ x => t
    | RApp Raw Raw            -- t u
    | RU                      -- U
    | RPi Name RTy RTy        -- (x : a) → b
    | RLet Name RTy Raw Raw   -- let x : a = t; u

  public export
  RTy : Type
  RTy = Raw

public export
data TopLevelRaw
  = TLet Name RTy Raw       -- let x : a = t
  | TPostulate Name RTy     -- posulate x : a
  -- inductive type
  -- data Nat
  -- | zero : Nat
  -- | suc : Nat -> Nat
  | TData Name (List (Name, RTy))

public export
data ModuleInfoRaw = MkModuleInfoRaw Name

public export
data ModuleRaw = MkModuleRaw ModuleInfoRaw (List TopLevelRaw)

export
Cast Raw Tm where
  cast (RSrcPos raw) = SrcPos $ MkBounded (cast raw.val) True raw.bounds
  cast (RVar x) = Var x
  cast (RLam x t) = Lam x (cast t)
  cast (RApp t u) = App (cast t) (cast u)
  cast RU = U
  cast (RPi x a b) = Pi x (cast a) (cast b)
  cast (RLet x a t u) = Let x (cast a) (cast t) (cast u)

toTTm : List TopLevelRaw -> Tm
toTTm [] = U
toTTm (TLet x a t :: xs) = Let x (cast a) (cast t) (toTTm xs)
toTTm (TPostulate x a :: xs) = Postulate x (cast a) (toTTm xs)
toTTm (TData x caseLst :: xs) = foldl (\a, (x, t) => \u => a (Postulate x (cast t) u))
  (\u => Postulate x U u)
  caseLst
  $ toTTm xs

export
Cast (List TopLevelRaw) Tm where
  cast = toTTm
