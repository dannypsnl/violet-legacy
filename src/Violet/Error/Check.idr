module Violet.Error.Check

import Text.Parser.Core
import Text.PrettyPrint.Prettyprinter.Doc
import Text.PrettyPrint.Prettyprinter.Render.Terminal

import Violet.Core.Term
import Violet.Error.Common

public export
data CheckErrorKind
  = NoVar String
  | InferLam Tm
  | BadApp Tm
  | TypeMismatch Tm Tm

prettyCheckErrorKind : CheckErrorKind -> Doc AnsiStyle
prettyCheckErrorKind (NoVar name) = bold' $ color' Red $ hsep ["variable:", pretty name, "not found"]
prettyCheckErrorKind (InferLam tm) = bold' $ color' Red $ hcat ["cannot inference lambda: ", pretty tm]
prettyCheckErrorKind (BadApp tm) = bold' $ color' Red $ hcat ["bad app on: ", pretty tm]
prettyCheckErrorKind (TypeMismatch t1 t2) = vcat
  [ bold' $ color' Red $ "type mismatched"
  , "expected type:"
  , bold' $ color' Blue $ indent 2 $ pretty t1
  , "actual type:"
  , bold' $ color' Yellow $ indent 2 $ pretty t2
  ]

public export
record CheckError where
  constructor MkCheckError
  filename, source : String
  bounds : Maybe Bounds
  errKind : CheckErrorKind

export
prettyCheckError : CheckError -> Doc AnsiStyle
prettyCheckError ce = case ce.bounds of
  Nothing => prettyCheckErrorKind ce.errKind
  Just bounds =>
    hcat [pretty ce.filename, ":", pretty (bounds.startLine+1), ":", pretty bounds.startCol, ":"]
    <++> line
    <++> line
    <++> getCode ce.source (cast bounds.startLine) bounds.endLine
    <++> line
    <++> line
    <++> prettyCheckErrorKind ce.errKind
