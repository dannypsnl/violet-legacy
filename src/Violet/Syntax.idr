module Violet.Syntax

import Data.String
import Text.Parser.Core
import public Violet.Core.Term

public export
data PatRaw
  = RPVar Name
  -- weak-head pattern
  | RPCons Name (List Name)

mutual
  ||| The Raw top-level definition of violet
  public export
  data TopLevelRaw
    = TSrcPos (WithBounds TopLevelRaw)
    -- def x : a => t
    | TDef Name RTy Raw
    -- data Nat
    -- | zero
    -- | suc Nat
    | TData Name (List RTy)
        -- constructors
        (List (Name, (List RTy)))

  ||| The Raw expression of violet
  public export
  data Raw
    = RSrcPos (WithBounds Raw)
    | RVar Name               -- x
    | RLam Name Raw           -- λ x => t
    | RApp Raw Raw            -- t u
    | RU                      -- U
    | RPi Name RTy RTy        -- (x : a) → b
    | RLet Name RTy Raw Raw   -- let x : a = t; u
    -- elim n
    -- | zero => u1
    -- | suc n => u2
    --
    -- elim trichotomy a b
    -- | greater _ => u1
    -- | less p | equals p => f p
    | RElim Raw (List (PatRaw, Raw))

  public export
  RTy : Type
  RTy = Raw

public export
data ModuleInfoRaw = MkModuleInfoRaw Name

public export
data ModuleRaw = MkModuleRaw ModuleInfoRaw (List TopLevelRaw)

export
Cast PatRaw Pat where
  cast (RPVar n) = PVar n
  cast (RPCons h vs) = PCons h vs

export
Cast Raw Tm where
  cast (RSrcPos raw) = SrcPos $ MkBounded (cast raw.val) True raw.bounds
  cast (RVar x) = Var x
  cast (RLam x t) = Lam x (cast t)
  cast (RApp t u) = Apply (cast t) (cast u)
  cast RU = U
  cast (RPi x a b) = Pi x (cast a) (cast b)
  cast (RLet x a t u) = Let x (cast a) (cast t) (cast u)
  cast (RElim r cases) = Elim (cast r) $ map (bimap cast cast) cases

export
Cast TopLevelRaw Definition where
  cast (TSrcPos top) = DSrcPos $ MkBounded (cast top.val) True top.bounds
  cast (TDef x a t) = Def x (cast a) (cast t)
  cast (TData x _ cases) = Data x (map toCase cases)
    where
      toCase : (Name, (List RTy)) -> DataCase
      toCase (x, tys) = C x (map cast tys)
