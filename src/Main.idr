module Main

import System
import System.File.Virtual
import Control.App
import Control.App.Handler
import Control.App.Console
import Control.App.FileIO
import Text.PrettyPrint.Prettyprinter.Symbols

import Violet.Elaboration
import Violet.Parser

prettyIOError : IOError -> Doc AnsiStyle
prettyIOError err = hsep $ map pretty ["error:", show err]

parseMod : HasErr PError e => String -> App e ModuleRaw
parseMod source = do
  Right raw <- pure $ parseViolet ruleModule source
    | Left err => throw err
  pure raw

checkMod : Has [PrimIO] e => String -> String -> ModuleRaw -> App e CheckState
checkMod filename source raw = do
	let ctx = (ctxFromFile filename source)
	new (checkState ctx) $ checkModule (map cast raw.tops) `handleErr` putErr prettyCheckError

loadModuleFile : (PrimIO e, FileIO (IOError :: e)) => String -> App e CheckState
loadModuleFile filename = do
	source <- readFile filename `handleErr` putErr prettyIOError
	raw <- parseMod source `handleErr` putErr prettyParsingError
	checkMod filename source raw

putCtx : PrimIO e => CheckState -> App e ()
putCtx state = do
	let env = MkEnv state.topEnv []
	for_ (reverse state.topCtx.map) $ \(name, ty) => do
		v <- new state (runQuote env ty) `handleErr` putErr prettyCheckError
		primIO $ putDoc $ (annotate bold $ pretty name)
			<++> ":"
			<++> (annBold $ annColor Blue $ pretty v)

runNf : Elab es => Env -> Tm -> App es Tm
runNf env a = do
	state <- getState
	Right b <- pure $ nf state.mctx env a
		| Left e => report $ cast e
	pure b

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

entry : (PrimIO e, FileIO (IOError :: e)) => List String -> App e ()
-- `violet check ./sample.vt`
entry ["check", filename] = loadModuleFile filename >>= putCtx
-- `violet ./sample.vt` will load `sample` into REPL
entry [filename] = loadModuleFile filename >>= \state => new state $ do replLoop
entry xs = primIO $ putDoc $ hsep [
		pretty "unknown command",
		dquotes $ hsep $ map pretty xs
	]

main : IO ()
main = run $ entry $ drop 1 !getArgs
