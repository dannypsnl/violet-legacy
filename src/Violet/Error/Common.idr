module Violet.Error.Common

import Data.String
import public Text.PrettyPrint.Prettyprinter.Doc
import public Text.PrettyPrint.Prettyprinter.Render.Terminal

export
getCode : (source : String) -> (line : Nat) -> (codeShift : Int) -> Doc AnsiStyle
getCode source line shift =
  let ls = lines source
  in case drop line ls of
    [] => pretty "bad line"
    (x :: xs) =>
      let curLine = x
      in (annotate bold $
          annotate (color Blue) $
          pretty (line+1)
          <++> pretty "|")
          <++> pretty curLine
