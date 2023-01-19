module Violet.Surface.Error

import Data.String
import Text.Parser
import Violet.Error.Common
import Violet.Surface.Lexer

public export
data PError : Type where
	CollectPError : (List PError) -> PError
	SinglePError : (source : String) -> (bounds : Maybe Bounds) -> (msg : String) -> PError
	TokensLeave : (source : String) -> (toks : List (WithBounds VToken)) ->  PError
	LexFail : PError

export
prettyParsingError : PError -> Doc AnsiStyle
prettyParsingError (CollectPError errs) = vsep $ map prettyParsingError errs
prettyParsingError LexFail = annColor Red $ "error: failed to lex"
prettyParsingError (TokensLeave source (tok :: toks)) = vsep [
		annColor Red $ "error: contains tokens that were not consumed",
		getCode source (cast tok.bounds.startLine) 2
	]
prettyParsingError (TokensLeave source []) = annColor Red $ "error: contains tokens that were not consumed"
prettyParsingError (SinglePError source Nothing msg) = annColor Red $ pretty msg
prettyParsingError (SinglePError source (Just bound) msg) =
	let shift = bound.endLine
	in getCode source (cast bound.startLine) shift
	<++> line
	<++> (annColor Red $
		indent shift (cat $ replicate (cast ((bound.endCol-1) - (bound.startCol-1))) "^")
		<++> "Parsing error:"
		<++> pretty msg)
