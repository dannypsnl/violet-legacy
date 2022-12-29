module Main

import System
import Control.App
import Control.App.Console
import Control.App.FileIO
import Text.PrettyPrint.Prettyprinter.Symbols

import Violet.Core
import Violet.Syntax
import Violet.Parser

putErr : PrimIO e => (err -> Doc AnsiStyle) -> err -> App e a
putErr prettyErr err = primIO $ do putDoc $ prettyErr err; exitSuccess

prettyIOError : IOError -> Doc AnsiStyle
prettyIOError err = hsep $ map pretty ["error:", show err]

parseMod : HasErr PError e => String -> App e Tm
parseMod source = do
  Right (MkModuleRaw _ tops) <- pure $ parseViolet ruleModule source
    | Left err => throw err
  pure $ cast tops
checkMod : PrimIO e => String -> String -> Tm -> App e (VTy, Env, Ctx)
checkMod filename source tm = do
  (vty, (env, ctx)) <- handle (infer' emptyEnv emptyCtx tm)
    pure (putErr prettyCheckError)
  pure (vty, env, ctx)

putCtx : PrimIO e => (VTy, Env, Ctx) -> App e ()
putCtx (ty, env, ctx) = do
  v <- handle (cquote ctx env ty) pure (putErr prettyCheckError)
  for_ ctx.map $ \(name, ty) => primIO $ putDoc $
    (annotate bold $ pretty name)
    <++> ":"
    <++> (annBold $ annColor Blue $ pretty v)

startREPL : PrimIO e => (VTy, Env, Ctx) -> App e ()
startREPL (_, env, ctx) = do
  putStr "> "
  src <- getLine
  Right raw <- pure $ parseViolet ruleTm src
    | Left err => putErr prettyParsingError err
  let tm = cast raw
  (ty, _) <- handle (infer' env ctx tm)
    pure (putErr prettyCheckError)
  v <- handle (cquote ctx env ty) pure (putErr prettyCheckError)
  primIO $ putDoc $ annBold (pretty tm)
    <++> ":"
    <++> (annBold $ annColor Blue $ pretty v)
  startREPL (ty, env, ctx)

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
