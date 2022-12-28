module Main

import System
import Control.App
import Control.App.FileIO
import Text.PrettyPrint.Prettyprinter.Symbols

import Violet.Core
import Violet.Syntax
import Violet.Parser

partial
checkMod : (PrimIO es, FileIO (IOError :: es)) => String -> App es (VTy, Env, Ctx)
checkMod filename = do
  source <- handle (readFile filename) pure
    (\err : IOError => do primIO $ putDoc $ hsep $ map pretty ["error:", show err]; idris_crash "")
  Right (MkModuleRaw _ tops) <- pure $ parse source
    | Left err => do primIO $ putDoc $ prettyParsingError err; idris_crash ""
  let tm = cast tops
  Right (vty, (env, ctx)) <- pure $ infer' emptyEnv (ctxFromFile filename source) tm
    | Left err => do primIO $ putDoc $ prettyCheckError err; idris_crash ""
  pure (vty, env, ctx)

putCtx : PrimIO es => (VTy, Env, Ctx) -> App es ()
putCtx (ty, env, ctx) = do
  for_ ctx.map $ \(name, ty) => primIO $ putDoc $
    (annotate bold $ pretty name)
    <++> ":"
    <++> (annBold $ annColor Blue $ pretty (quote env ty))

startREPL : PrimIO es => (VTy, Env, Ctx) -> App es ()
startREPL (ty, env, ctx) = ?todo

partial
entry : (PrimIO es, FileIO (IOError :: es)) => List String -> App es ()
entry ["check", filename] = (checkMod filename) >>= putCtx
entry [filename] = (checkMod filename) >>= startREPL
entry xs = primIO $ putDoc $ hsep [
    pretty "unknown command",
    dquotes $ hsep $ map pretty xs
  ]

partial
main : IO ()
main = run $ entry $ drop 1 !getArgs
