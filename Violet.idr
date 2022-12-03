module Violet

import Effects
import Effect.StdIO
import Effect.File

import public Lightyear.StringFile

import Violet.Syntax
import Violet.Parser
import Violet.Core

data FuseError
    -- filename, error
  = FileE String FileError
  | ParseE String String

Show FuseError where
  show (FileE filename _) = "fail to read" ++ filename
  show (ParseE filename msg) = filename ++ " " ++ msg

export
handle : List String -> Eff () [FILE(), STDIO]
handle ["check", filename] = do
  case !(parseFile FileE ParseE violetSrc filename) of
    Left err => putStrLn $ "error: " ++ show err
    Right tm =>
      case (infer emptyEnv emptyCtx tm) of
        Left ce => putStrLn $ show ce
        Right _ => putStrLn $ show tm
handle _ = pure ()
