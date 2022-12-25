module Violet.Error.Parsing

import Text.PrettyPrint.Prettyprinter.Doc
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Text.Parser
import Violet.Lexer

public export
data PError = MkPError String (ParsingError VToken)

export
prettyError : PError -> Doc ann
prettyError (MkPError source (Error str Nothing)) = pretty str
prettyError (MkPError source (Error str (Just bound))) = pretty str
