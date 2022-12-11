module Violet

import Text.Lexer
import public Text.Parser.Core
import public Text.Parser

import Violet.Core
import Violet.Syntax
import Violet.Lexer
import Violet.Parser

parseString : String -> Either (ParseError (TokenData VToken)) (Tm, List (TokenData VToken))
parseString s = parse tmFull $ fst (skipWs (lex violetTokens s))

export
handle : List String -> Eff () [FILE(), STDIO]
handle ["check", filename] = do
  case !(readFile filename) of
    FError err => putStrLn $ "error: " ++ show err
    Result fileContent =>
      case (parseString fileContent) of
        -- TODO: print err
        Left err => putStrLn $ "error"
        Right (tm, _) =>
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
