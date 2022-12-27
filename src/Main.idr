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
    (\source =>
      case (parse source) of
        Left pErr => primIO $ putDoc $ prettyError pErr
        Right (MkModuleRaw _ xs) =>
          let tm = cast xs
          in case (infer' emptyEnv (ctxFromFile filename source) tm) of
            Left cErr => primIO $ putDoc $
              (annotate bold $ pretty (nf0 tm))
              <++> "has error:"
              <++> line
              <++> prettyCheckError cErr
            Right (vty, (env, ctx)) =>
              for_ ctx.map $ \(name, ty) =>
                primIO $ putDoc $
                  (annotate bold $ pretty name)
                  <++> ":"
                  <++> (annotate bold $ annotate (color Blue) $ pretty (quote env ty))
      )
    (\err : IOError => putStrLn $ "error: " ++ show err)
handle _ = pure ()

main : IO ()
main = run $ handle !getArgs
