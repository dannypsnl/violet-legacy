module Violet

import Effects
import Effect.StdIO
import Effect.File

import public Lightyear.StringFile

import Violet.Syntax
import Violet.Parser

data FuseError
    -- filename, error
  = FileE String FileError
  | ParseE String String

Show FuseError where
  show (FileE _ _) = "file error"
  show (ParseE filename msg) = filename ++ "\n" ++ msg

export
handle : List String -> Eff () [FILE(), STDIO]
handle ["check", filename] = do
  case !(parseFile FileE ParseE violetSrc filename) of
    Left err => putStrLn $ show err
    Right tm => putStrLn "checking"
handle _ = pure ()
