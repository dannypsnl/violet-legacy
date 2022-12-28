module Violet.Error.Common

import public Data.String
import public Text.PrettyPrint.Prettyprinter.Doc
import public Text.PrettyPrint.Prettyprinter.Render.Terminal

export
annBold : Doc AnsiStyle -> Doc AnsiStyle
annBold = annotate bold

export
annColor : Color -> Doc AnsiStyle -> Doc AnsiStyle
annColor = annotate . color

export
getCode : (source : String) -> (line : Nat) -> (codeShift : Int) -> Doc AnsiStyle
getCode source line shift =
  let ls = lines source
  in case drop line ls of
    [] => pretty "bad line"
    (x :: xs) =>
      let curLine = x
      in (annBold $ annColor Blue $
          pretty (line+1)
          <++> pretty "|")
          <++> pretty curLine
