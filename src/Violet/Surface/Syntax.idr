module Violet.Surface.Syntax

import Data.String
import Data.List1
import Text.Parser.Core
import public Violet.Core.Syntax

mutual
	public export
	RTelescope : Type
	RTelescope = List (Mode, Name, RTy)

	||| The Raw top-level definition of violet
	public export
	data TopLevelRaw
		= TSrcPos (WithBounds TopLevelRaw)
		-- def x : a => t
		| TDef Name RTelescope RTy Raw
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
		| RPi Mode Name RTy RTy   -- (x : a) → b | {x : a} → b
		| RHole Name              -- ?x
		| RLet Name RTy Raw Raw   -- let x : a = t; u
		-- elim n
		-- | zero => u1
		-- | suc n => u2
		--
		-- elim n, m
		-- | zero, m => m
		-- | n, zero => n
		-- | suc n, suc m => suc $ suc $ plus n m
		--
		-- or pattern example:
		--
		--   elim trichotomy a b
		--   | greater _ => u1
		--   | less p | equals p => f p
		| RElim (List Raw) (List ElimCase)

	public export
	ElimCase : Type
	ElimCase = (List Pat, Raw)

	public export
	Pat : Type
	Pat = List1 Name

	public export
	RTy : Type
	RTy = Raw

public export
data ModuleImportStmt = MkModuleImportStat Name -- imported module name

public export
record ModuleInfoRaw where
  constructor MkModuleInfoRaw
  name : string
  imports : List ModuleImportStmt

public export
data ModuleRaw = MkModuleRaw ModuleInfoRaw (List TopLevelRaw)

export
Cast Raw STm where
	cast (RSrcPos raw) = SrcPos $ MkBounded (cast raw.val) True raw.bounds
	cast (RVar x) = SVar x
	cast (RLam x t) = SLam x (cast t)
	cast (RApp t u) = SApply (cast t) (cast u)
	cast RU = SU
	cast (RPi mode x a b) = SPi mode x (cast a) (cast b)
	cast (RHole x) = Hole x
	cast (RLet x a t u) = SLet x (cast a) (cast t) (cast u)
	cast (RElim rs cases) = SElim (map cast rs) $ map (bimap (map (\x => x)) cast) cases

export
Cast TopLevelRaw Definition where
	cast (TSrcPos top) = DSrcPos $ MkBounded (cast top.val) True top.bounds
	cast (TDef x tele a t) = Def x (toPi tele $ cast a) (toLam tele $ cast t)
	where
		toPi : RTelescope -> STm -> STm
		toPi ((mode, x, ty) :: tele) t = SPi mode x (cast ty) (toPi tele t)
		toPi [] t = t

		toLam : RTelescope -> STm -> STm
		toLam ((mode, x, _) :: tele) t = SLam x (toLam tele t)
		toLam [] t = t
	cast (TData x _ cases) = Data x (map toCase cases)
		where
			toCase : (Name, (List RTy)) -> DataCase
			toCase (x, tys) = C x (map cast tys)
