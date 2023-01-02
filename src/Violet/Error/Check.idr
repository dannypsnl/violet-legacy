module Violet.Error.Check

import Text.Bounded
import Text.PrettyPrint.Prettyprinter.Doc
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Violet.Core.Term
import public Violet.Error.Common
import Violet.Error.Eval

public export
data CheckErrorKind
  = NoVar String
  | InferLam Tm
  | BadApp Tm
  | TypeMismatch Tm Tm
  | EvalE EvalError

export
Cast EvalError CheckErrorKind where
  cast e = EvalE e

prettyCheckErrorKind : CheckErrorKind -> Doc AnsiStyle
prettyCheckErrorKind (NoVar name) = annBold $ annColor Red $ hsep ["variable:", pretty name, "not found"]
prettyCheckErrorKind (InferLam tm) = annBold $ annColor Red $ hsep ["cannot inference lambda:", pretty tm]
prettyCheckErrorKind (BadApp tm) = annBold $ annColor Red $ hsep ["bad app on:", pretty tm]
prettyCheckErrorKind (TypeMismatch t1 t2) = vcat
  [ annBold $ annColor Red $ "type mismatched"
  , "expected type:"
  , annBold $ annColor Blue $ indent 2 $ pretty t1
  , "actual type:"
  , annBold $ annColor Yellow $ indent 2 $ pretty t2
  ]
prettyCheckErrorKind (EvalE e) = prettyEvalError e

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
