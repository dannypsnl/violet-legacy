module Violet.Syntax

import Data.String
import Text.Parser.Core
import public Violet.Core.Term

public export
data PatRaw = RPat Name (List Name)

mutual
	public export
	RTelescope : Type
	RTelescope = List (Name, RTy)

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
		| RPi Name RTy RTy        -- (x : a) → b
		| RLet Name RTy Raw Raw   -- let x : a = t; u
		-- elim n
		-- | zero => u1
		-- | suc n => u2
		--
		-- elim trichotomy a b
		-- | greater _ => u1
		-- | less p | equals p => f p
		| RElim (List Raw) (List (List PatRaw, Raw))

	public export
	RTy : Type
	RTy = Raw

public export
data ModuleInfoRaw = MkModuleInfoRaw Name

public export
data ModuleRaw = MkModuleRaw ModuleInfoRaw (List TopLevelRaw)

export
Cast PatRaw Pat where
	cast (RPat h vs) = PCtor h vs

export
Cast Raw Tm where
	cast (RSrcPos raw) = SrcPos $ MkBounded (cast raw.val) True raw.bounds
	cast (RVar x) = Var x
	cast (RLam x t) = Lam x (cast t)
	cast (RApp t u) = Apply (cast t) (cast u)
	cast RU = U
	cast (RPi x a b) = Pi x (cast a) (cast b)
	cast (RLet x a t u) = Let x (cast a) (cast t) (cast u)
	cast (RElim rs cases) = Elim (map cast rs) $ map (bimap (map cast) cast) cases

export
Cast TopLevelRaw Definition where
	cast (TSrcPos top) = DSrcPos $ MkBounded (cast top.val) True top.bounds
	cast (TDef x tele a t) = Def x (toPi tele $ cast a) (toLam tele $ cast t)
	where
		toPi : RTelescope -> Tm -> Tm
		toPi ((x, ty) :: tele) t = Pi x (cast ty) (toPi tele t)
		toPi [] t = t

		toLam : RTelescope -> Tm -> Tm
		toLam ((x, _) :: tele) t = Lam x (toLam tele t)
		toLam [] t = t
	cast (TData x _ cases) = Data x (map toCase cases)
		where
			toCase : (Name, (List RTy)) -> DataCase
			toCase (x, tys) = C x (map cast tys)
