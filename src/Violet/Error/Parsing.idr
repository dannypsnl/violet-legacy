module Violet.Error.Parsing

import Data.String
import Text.Parser

import Violet.Error.Common
import Violet.Lexer

repeat : Nat -> Doc ann -> List (Doc ann)
repeat n s = iterateN n (\x => x) s

export
record PError where
  constructor MkPError
  source : String
  bounds : Maybe Bounds
  msg : String

export
pErrFromStr : (source : String) -> String -> PError
pErrFromStr source msg = MkPError source Nothing msg

export
fromParsingError : (source : String) -> ParsingError VToken -> PError
fromParsingError source (Error msg bounds) = MkPError source bounds msg

export
prettyError : PError -> Doc AnsiStyle
prettyError (MkPError source Nothing msg) = pretty msg
prettyError (MkPError source (Just bound) msg) =
  let shift = bound.endLine
  in getCode source (cast bound.startLine) shift
  <++> line
  <++> (annotate (color Red) $
      indent shift (cat $ repeat (cast ((bound.endCol-1) - (bound.startCol-1))) "^")
      <++> "Parsing error:"
      <++> pretty msg)
