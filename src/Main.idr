module Main

import System
import Control.App
import Control.App.Console
import Control.App.FileIO
import Text.PrettyPrint.Prettyprinter.Symbols

import Violet.Core
import Violet.Syntax
import Violet.Parser

checkState : Ctx -> Env -> CheckState
checkState ctx env = MkCheckState ctx env

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

startREPL : PrimIO e => CheckState -> App e ()
startREPL state = do
  putStr "> "
  src <- getLine
  Right raw <- pure $ parseViolet ruleTm src
    | Left err => putErr prettyParsingError err
  let tm = cast raw
  (ty, s) <- handle (new state $ infer' tm) pure (putErr prettyCheckError)
  v <- handle (new state (runEval quote s.topEnv ty)) pure (putErr prettyCheckError)
  primIO $ putDoc $ annBold (pretty tm)
    <++> ":"
    <++> (annBold $ annColor Blue $ pretty v)
  startREPL s

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
  checkMod filename source defs >>= startREPL
entry xs = primIO $ putDoc $ hsep [
    pretty "unknown command",
    dquotes $ hsep $ map pretty xs
  ]

main : IO ()
main = run $ entry $ drop 1 !getArgs
