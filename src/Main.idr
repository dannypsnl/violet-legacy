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

parseMod : HasErr PError e => String -> App e Tm
parseMod source = do
  Right (MkModuleRaw _ tops) <- pure $ parseViolet ruleModule source
    | Left err => throw err
  pure $ cast tops
checkMod : Has [PrimIO] e => String -> String -> Tm -> App e (VTy, CheckState)
checkMod filename source tm = do
  let ctx = (ctxFromFile filename source)
      env = emptyEnv
  (vty, state) <- handle (new (checkState ctx env) $ infer' tm)
    pure (putErr prettyCheckError)
  pure (vty, state)

putCtx : PrimIO e => (VTy, CheckState) -> App e ()
putCtx (ty, state) = do
  for_ (reverse state.topCtx.map) $ \(name, ty) => do
    v <- handle (new state (runEval quote state.topEnv ty)) pure (putErr prettyCheckError)
    primIO $ putDoc $ (annotate bold $ pretty name)
      <++> ":"
      <++> (annBold $ annColor Blue $ pretty v)

startREPL : PrimIO e => (VTy, CheckState) -> App e ()
startREPL (_, state) = do
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
  startREPL (ty, s)

entry : (PrimIO e, FileIO (IOError :: e)) => List String -> App e ()
-- `violet check ./sample.vt`
entry ["check", filename] = do
  source <- handle (readFile filename) pure (putErr prettyIOError)
  tm <- handle (parseMod source) pure (putErr prettyParsingError)
  checkMod filename source tm >>= putCtx
-- `violet ./sample.vt` will load `sample` into REPL
entry [filename] = do
  source <- handle (readFile filename) pure (putErr prettyIOError)
  tm <- handle (parseMod source) pure (putErr prettyParsingError)
  checkMod filename source tm >>= startREPL
entry xs = primIO $ putDoc $ hsep [
    pretty "unknown command",
    dquotes $ hsep $ map pretty xs
  ]

main : IO ()
main = run $ entry $ drop 1 !getArgs
