module Violet.Error.Common

import public Data.String
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

export
bold' : Doc AnsiStyle -> Doc AnsiStyle
bold' = annotate bold

export
color' : Color -> Doc AnsiStyle -> Doc AnsiStyle
color' = annotate . color
