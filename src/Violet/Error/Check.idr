module Violet.Error.Check

import Text.Parser.Core
import Text.PrettyPrint.Prettyprinter.Doc
import Text.PrettyPrint.Prettyprinter.Render.Terminal

public export
data CheckError ann = MkCheckError (Maybe Bounds) (Doc ann)

export
prettyCE : {ann : Type} -> CheckError ann -> Doc ann
prettyCE (MkCheckError Nothing msg) = msg
prettyCE (MkCheckError (Just bounds) msg) =
    hcat [pretty bounds.startLine, ":", pretty bounds.startCol, ":"]
      <++> line
      <++> msg
