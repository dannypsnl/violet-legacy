module Main

import System
import Control.App
import Control.App.Handler
import Control.App.Console
import Control.App.FileIO
import Text.PrettyPrint.Prettyprinter.Symbols

import Violet.Elaboration
import Violet.Parser
import Violet.Surface.Syntax

import CLI.Check

replLoop : Has [PrimIO, State CheckState CheckState] e => App e ()
replLoop = do
  putStr "> "
  Right raw <- pure $ parseViolet ruleTm !(getLine)
    | Left err => putErr prettyParsingError err
  let tm = cast raw
  state <- get CheckState
  let env = MkEnv state.topEnv []
  (tm, t) <- infer env state.topCtx tm `handleErr` putErr prettyCheckError
  ty <- runQuote env t `handleErr` putErr prettyCheckError
  v <- runNf env tm `handleErr` putErr prettyCheckError
  primIO $ putDoc $ hsep [annBold (pretty v), ":", (annBold $ annColor Blue $ pretty ty)]
  replLoop
  where
    runNf : Elab es => Env -> Tm -> App es Tm
    runNf env a = do
      state <- getState
      Right b <- pure $ nf state.mctx env a
        | Left e => report $ cast e
      pure b

entry : (PrimIO e, FileIO (IOError :: e)) => List String -> App e ()
-- `violet check ./sample.vt`
entry ("check" :: options) = checkCommand defaultCheckOpts options
-- FIXME: uncomment this command, the idea is simple: new REPL will base on binary format
--        , and hence this must wait `.vto` format (issue #80) to bring this back
-- `violet ./sample.vt` will load `sample` into REPL
-- this command implies checking
--entry [filename] = loadModuleFile filename >>= \state => new state $ do replLoop
entry xs = primIO $ putDoc $ hsep [
    pretty "unknown command",
    dquotes $ hsep $ map pretty xs
  ]

main : IO ()
main = run $ entry $ drop 1 !getArgs
