module Violet.Core.Syntax

import Data.String
import public Text.Bounded
import Text.PrettyPrint.Prettyprinter.Doc
import Text.PrettyPrint.Prettyprinter.Symbols
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import public Violet.Core.Common

mutual
	public export
	data STm
		= SrcPos (WithBounds STm)
		| SVar Name                -- x
		| SLam Name STm            -- λ x => t
		| SApply STm STm           -- t u
		| SU                       -- U
		| SPi Mode Name STy STy    -- (x : a) → b | {x : a} → b
		| Hole Name                -- ?x
		| SLet Name STy STm STm    -- let x : a = t; u
		| SElim (List STm) (List SElimCase)

	public export
	STy : Type
	STy = STm

	public export
	SElimCase : Type
	SElimCase = (List (List1 Name), STm)

	public export
	data DataCase = C Name (List STy)

	public export
	data Definition
		= DSrcPos (WithBounds Definition)
		-- data Nat
		-- | zero
		-- | suc Nat
		| Data Name (List DataCase)
		-- let x : a = t;
		| Def Name STy STm
