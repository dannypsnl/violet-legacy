module Violet.Error.Check

import Text.Parser.Core

import Violet.Error.Common

public export
record CheckError ann where
  constructor MkCheckError
  filename, source : String
  bounds : Maybe Bounds
  msg : Doc ann

export
prettyCheckError : CheckError AnsiStyle -> Doc AnsiStyle
prettyCheckError ce = case ce.bounds of
  Nothing => ce.msg
  Just bounds =>
    hcat [pretty ce.filename, ":", pretty (bounds.startLine+1), ":", pretty bounds.startCol, ":"]
    <++> line
    <++> line
    <++> getCode ce.source (cast bounds.startLine) bounds.endLine
    <++> line
    <++> line
    <++> ce.msg
