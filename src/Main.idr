module Main

import System
import System.File.Virtual
import Control.App
import Control.App.Console
import Control.App.FileIO
import Text.PrettyPrint.Prettyprinter.Symbols

import Violet.Core
import Violet.Syntax
import Violet.Parser

prettyIOError : IOError -> Doc AnsiStyle
prettyIOError err = hsep $ map pretty ["error:", show err]

parseMod : HasErr PError e => String -> App e (List Definition)
parseMod source = do
  Right (MkModuleRaw _ tops) <- pure $ parseViolet ruleModule source
    | Left err => throw err
  pure $ map cast tops
checkMod : Has [PrimIO] e => String -> String -> List Definition -> App e CheckState
checkMod filename source defs = do
  let ctx = (ctxFromFile filename source)
      env = emptyEnv
  handle (new (checkState ctx env) $ checkModule defs) pure (putErr prettyCheckError)

putCtx : PrimIO e => CheckState -> App e ()
putCtx state = do
  for_ (reverse state.topCtx.map) $ \(name, ty) => do
    v <- handle (new state (runEval quote state.topEnv ty)) pure (putErr prettyCheckError)
    primIO $ putDoc $ (annotate bold $ pretty name)
      <++> ":"
      <++> (annBold $ annColor Blue $ pretty v)

replLoop : PrimIO e => CheckState -> App e ()
replLoop state = do
  putStr "> "
  src <- getLine
  Right raw <- pure $ parseViolet ruleTm src
    | Left err => putErr prettyParsingError err
  let tm = cast raw
  ty <- handle (new state $ do
    t <- infer' tm
    runEval quote state.topEnv t)
    pure (putErr prettyCheckError)
  v <- handle (new state $ do
    v <- runEval eval state.topEnv tm
    runEval quote state.topEnv v)
    pure (putErr prettyCheckError)
  primIO $ putDoc $
    hsep [annBold (pretty v), ":", (annBold $ annColor Blue $ pretty ty)]
  replLoop state

entry : (PrimIO e, FileIO (IOError :: e)) => List String -> App e ()
-- `violet check ./sample.vt`
entry ["check", filename] = do
  source <- handle (readFile filename) pure (putErr prettyIOError)
  defs <- handle (parseMod source) pure (putErr prettyParsingError)
  checkMod filename source defs >>= putCtx
-- `violet ./sample.vt` will load `sample` into REPL
entry [filename] = do
  source <- handle (readFile filename) pure (putErr prettyIOError)
  defs <- handle (parseMod source) pure (putErr prettyParsingError)
  checkMod filename source defs >>= replLoop
entry xs = primIO $ putDoc $ hsep [
    pretty "unknown command",
    dquotes $ hsep $ map pretty xs
  ]

main : IO ()
main = run $ entry $ drop 1 !getArgs
