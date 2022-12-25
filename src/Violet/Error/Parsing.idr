module Violet.Error.Parsing

import Data.String
import Text.Parser

import Violet.Error.Common
import Violet.Lexer

repeat : Nat -> Doc ann -> List (Doc ann)
repeat n s = iterateN n (\x => x) s

public export
record PError where
  constructor MkPError
  source : String
  error : ParsingError VToken

export
prettyError : PError -> Doc AnsiStyle
prettyError (MkPError source (Error str Nothing)) = pretty str
prettyError (MkPError source (Error str (Just bound))) =
  let shift = bound.endLine
  in getCode source (cast bound.startLine) shift
  <++> line
  <++> (annotate (color Red) $
      indent shift (cat $ repeat (cast ((bound.endCol-1) - (bound.startCol-1))) "^")
      <++> "Parsing error:"
      <++> pretty str)
