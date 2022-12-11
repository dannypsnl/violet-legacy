module Main

import System
import Control.App
import Control.App.Console
import Control.App.FileIO
import Data.String

import Violet.Core
import Violet.Syntax
import Violet.Parser

export
handle : (FileIO (IOError :: es), Console es) => List String -> App es ()
handle [_, "check", filename] =
  handle (readFile filename)
    (\fileContent =>
      case (parse fileContent) of
        Left msg => putStrLn msg
        Right tm =>
          case (infer emptyEnv emptyCtx tm) of
            Left ce => putStr $ unlines [ "term:\n", show tm, "\nhas error:\n", show ce]
            Right vty => putStr $ unlines [ "term:\n", show tm, "\nhas type:\n", show $ quote emptyEnv vty]
      )
    (\err : IOError => putStrLn $ "error: " ++ show err)
handle _ = pure ()

main : IO ()
main = run $ handle !getArgs
