module Main

import System
import Control.App
import Control.App.Console
import Control.App.FileIO
import Control.Monad.Error.Either
import Data.String
import Text.PrettyPrint.Prettyprinter.Doc
import Text.PrettyPrint.Prettyprinter.Symbols
import Text.PrettyPrint.Prettyprinter.Render.Terminal

import Violet.Core
import Violet.Syntax
import Violet.Parser

entry : (PrimIO es, FileIO (IOError :: es), Console es) => List String -> App es ()
entry ["check", filename] = handle (readFile filename)
  (\source => do
    Right (MkModuleRaw _ xs) <- pure $ parse source
      | Left pErr => primIO $ putDoc $ prettyError pErr
    let tm = cast xs
    Right (vty, (env, ctx)) <- pure $ infer' emptyEnv (ctxFromFile filename source) tm
      | Left cErr => primIO $ putDoc $
        (annBold $ pretty (nf0 tm))
        <++> "has error:"
        <++> line
        <++> prettyCheckError cErr
    for_ ctx.map $ \(name, ty) =>
      primIO $ putDoc $
      (annotate bold $ pretty name)
      <++> ":"
      <++> (annBold $ annColor Blue $ pretty (quote env ty))
    )
  (\err : IOError => primIO $ putDoc $ hsep $ map pretty ["error:", show err])
entry xs = primIO $ putDoc $ hsep [
    pretty "unknown command",
    dquotes $ hsep $ map pretty xs
  ]

main : IO ()
main = run $ entry $ drop 1 !getArgs
