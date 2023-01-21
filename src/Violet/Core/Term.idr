module Violet.Core.Term

import Data.String
import public Text.Bounded
import Text.PrettyPrint.Prettyprinter.Doc
import Text.PrettyPrint.Prettyprinter.Symbols
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import public Violet.Core.Common

public export
data Pat
	-- var pattern
	= PVar Name
	-- ctor pattern
	| PCtor Name (List Name)

mutual
	||| The Core Term of violet language
	public export
	data Tm
		= Var Name             -- x
		| Lam Name Tm          -- λ x => t
		| Apply Tm Tm          -- t u
		| U                    -- U
		| Pi Mode Name Ty Ty   -- (x : a) → b
		| Let Name Ty Tm Tm    -- let x : a = t; u
		| Elim (List Tm) (List ElimCase)
		| Meta MetaVar

	public export
	Ty : Type
	Ty = Tm

	public export
	data ElimCase = ECase (List Pat) Tm

export
Pretty Pat where
	pretty (PVar x) = pretty x
	pretty (PCtor h vs) = pretty h <++> hsep (map pretty vs)

export
Pretty Tm where
	pretty (Var name) = pretty name
	pretty (Lam x t) = hsep ["λ", pretty x, "=>", pretty t]
	pretty (Apply t u) = pretty t <++> case u of
		Apply {} => parens $ pretty u
		_        => pretty u
	pretty U = "U"
	pretty (Pi mode x a b) =
		(if x == "_"
			then pretty a
			else case mode of
				Implicit => "{" <+> pretty x <++> ":" <++> pretty a <+> "}"
				Explicit => "(" <+> pretty x <++> ":" <++> pretty a <+> ")")
		<++> "→"
		<++> pretty b
	pretty (Let x a t u) =
		hsep [ "let", pretty x, ":", pretty a, "=", hcat [pretty t, ";"] ]
		<++> line
		<++> pretty u
	pretty (Elim tm cases) =
		"elim" <++> pretty tm
		<++> line
		<++> vsep (map prettyCase cases)
		where
			prettyCase : ElimCase -> Doc ann
			prettyCase (ECase ps t) = pipe <++> (encloseSep emptyDoc emptyDoc comma $ map pretty ps) <++> "=>" <++> pretty t
	pretty (Meta n) = hcat ["?", pretty n]
