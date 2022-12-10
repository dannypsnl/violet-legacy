module Violet

import Effects
import Effect.StdIO
import Effect.File

import public Lightyear.StringFile

import Violet.Core
import Violet.Syntax
import Violet.CParser
import Violet.Parser

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
        Left ce => do
          putStr $ unlines
            [ "term:\n"
            , show tm
            , "\nhas error:\n"
            , show ce
            ]
        Right vty => do
          putStr $ unlines
            [ "term:\n"
            , show tm
            , "\nhas type:\n"
            , show $ quote emptyEnv vty
            ]
handle _ = pure ()
