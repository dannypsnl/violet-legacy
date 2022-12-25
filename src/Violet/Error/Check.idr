module Violet.Error.Check

import Text.Parser.Core

import Violet.Error.Common

public export
data CheckError ann = MkCheckError (Maybe Bounds) (Doc ann)

export
prettyCE : String -> String -> CheckError AnsiStyle -> Doc AnsiStyle
prettyCE filename source (MkCheckError Nothing msg) = msg
prettyCE filename source (MkCheckError (Just bounds) msg) =
  hcat [pretty filename, ":", pretty (bounds.startLine+1), ":", pretty bounds.startCol, ":"]
  <++> line
  <++> line
  <++> getCode source (cast bounds.startLine) bounds.endLine
  <++> line
  <++> line
  <++> msg
