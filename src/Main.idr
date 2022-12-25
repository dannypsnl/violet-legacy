module Main

import System
import Control.App
import Control.App.Console
import Control.App.FileIO
import Data.String
import Text.PrettyPrint.Prettyprinter.Doc
import Text.PrettyPrint.Prettyprinter.Render.Terminal

import Violet.Core
import Violet.Syntax
import Violet.Parser

export
handle : (PrimIO es, FileIO (IOError :: es), Console es) => List String -> App es ()
handle [_, "check", filename] =
  handle (readFile filename)
    (\fileContent =>
      case (parse fileContent) of
        Left doc => primIO $ putDoc doc
        Right raw =>
          let tm = (toTm raw)
          in case (infer emptyEnv emptyCtx tm) of
            Left ce => primIO $ putDoc $
              (annotate bold $ pretty (show (nf0 tm)))
              <++> "has error:"
              <++> line
              <++> hcat [pretty filename, ":", prettyCE ce]
            Right vty => primIO $ putDoc $
              (annotate bold $ pretty (show (nf0 tm)))
              <++> ":"
              <++> (annotate bold $ annotate (color Blue) $ pretty (show (quote emptyEnv vty)))
      )
    (\err : IOError => putStrLn $ "error: " ++ show err)
handle _ = pure ()

main : IO ()
main = run $ handle !getArgs
