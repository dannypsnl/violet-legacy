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
checkMod : HasErr CheckError e => String -> String -> Tm -> App e (VTy, Env, Ctx)
checkMod filename source tm = do
  Right (vty, (env, ctx)) <- pure $ infer' emptyEnv (ctxFromFile filename source) tm
    | Left err => throw err
  pure (vty, env, ctx)

putCtx : PrimIO e => (VTy, Env, Ctx) -> App e ()
putCtx (ty, env, ctx) = for_ ctx.map $ \(name, ty) => primIO $ putDoc $
  (annotate bold $ pretty name)
  <++> ":"
  <++> (annBold $ annColor Blue $ pretty (quote env ty))

startREPL : Has [PrimIO, Console] e => (VTy, Env, Ctx) -> App e ()
startREPL (_, env, ctx) = do
  putStr "> "
  src <- getLine
  Right raw <- pure $ parseViolet ruleTm src
    | Left err => putErr prettyParsingError err
  let tm = cast raw
  Right (ty, _) <- pure $ infer' env ctx tm
    | Left err => putErr prettyCheckError err
  primIO $ putDoc $ annBold (pretty tm)
    <++> ":"
    <++> (annBold $ annColor Blue $ pretty (quote env ty))
  startREPL (ty, env, ctx)

entry : (PrimIO e, FileIO (IOError :: e)) => List String -> App e ()
-- `violet check ./sample.vt`
entry ["check", filename] = do
  source <- handle (readFile filename) pure (putErr prettyIOError)
  tm <- handle (parseMod source) pure (putErr prettyParsingError)
  handle (checkMod filename source tm) pure (putErr prettyCheckError) >>= putCtx
-- `violet ./sample.vt` will load `sample` into REPL
entry [filename] = do
  source <- handle (readFile filename) pure (putErr prettyIOError)
  tm <- handle (parseMod source) pure (putErr prettyParsingError)
  handle (checkMod filename source tm) pure (putErr prettyCheckError) >>= startREPL
entry xs = primIO $ putDoc $ hsep [
    pretty "unknown command",
    dquotes $ hsep $ map pretty xs
  ]

main : IO ()
main = run $ entry $ drop 1 !getArgs
