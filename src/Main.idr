module Main

import System
import System.File.Virtual
import System.Path
import Data.String
import Data.SortedSet
import Data.SortedMap
import Libraries.Data.Graph
import Control.App
import Control.App.Handler
import Control.App.Console
import Control.App.FileIO
import Text.PrettyPrint.Prettyprinter.Symbols

import Violet.Elaboration
import Violet.Parser
import Violet.Surface.Syntax

prettyIOError : IOError -> Doc AnsiStyle
prettyIOError err = hsep $ map pretty ["error:", show err]

parseMod : HasErr PError e => String -> App e ModuleRaw
parseMod source = do
  Right raw <- pure $ parseViolet ruleModule source
    | Left err => throw err
  pure raw

checkMod
  : Has [PrimIO] e
  => CheckState
  -> String
  -> String
  -> ModuleRaw
  -> App e CheckState
checkMod carriedCheckState filename source raw = do
  let ctx = (ctxFromFile filename source)
  new (initCheckStateWithCarriedState carriedCheckState ctx) 
    $ checkModule (map cast raw.tops) `handleErr` putErr prettyCheckError

moduleNameToFilepath : String -> Name -> String
moduleNameToFilepath root moduleName =
  (joinPath $ forget $ cons root $ split (== '.') moduleName) <.> ".vt"

getImports : ModuleRaw -> SortedSet Name
getImports raw =
  fromList $ map (\(MkModuleImportStmt name) => name) raw.info.imports

parseModuleFile
  : (PrimIO e, FileIO (IOError :: e))
  => String
  -> App e (ModuleRaw, String)
parseModuleFile filename = do
  primIO $ putStrLn $ "parse module " ++ filename
  source <- readFile filename `handleErr` putErr prettyIOError
  raw <- parseMod source `handleErr` putErr prettyParsingError
  pure (raw, source)

parseDepModules
  : (PrimIO e, FileIO (IOError :: e))
  => String
  -> SortedMap Name (ModuleRaw, SortedSet Name, String)
  -> List Name
  -> App e (SortedMap Name (ModuleRaw, SortedSet Name, String))
parseDepModules root acc [] = pure acc
parseDepModules root acc (moduleName :: rest) =
  case lookup moduleName acc of
    Just _ => parseDepModules root acc rest
    Nothing => do
      (raw, source) <-
        parseModuleFile $ moduleNameToFilepath root moduleName
      let imports = getImports raw
      let acc' = insert moduleName (raw, imports, source) acc
      parseDepModules root acc' (rest ++ Data.SortedSet.toList imports)

loadModuleFile : (PrimIO e, FileIO (IOError :: e)) => String -> App e CheckState
loadModuleFile filename = do
  -- TOOD: make root folder configerable
  let rootPath = "./example"
  modules <- parseDepModules rootPath empty $ singleton filename
  let topoSortedLists = tarjan $ (\(_, imports, _) => imports) <$> modules
  let
    go : CheckState -> List1 Name -> App e CheckState
    go accCheckState component = do
      -- currently only allow one module per component
      let moduleName = head component
      let currFilename = moduleNameToFilepath rootPath moduleName
      (raw, source) <-
        case SortedMap.lookup moduleName modules of
          Nothing => parseModuleFile currFilename -- fallback
          Just (raw, _, source) => pure (raw, source)
      primIO $ putStrLn $ "checking module " ++ moduleName
      checkMod accCheckState currFilename source raw
  foldlM go (checkState emptyCtx) $ List.reverse topoSortedLists

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
entry ["check"] =  primIO $ putDoc $ pretty "missing file, usage `violet check <file>`"
entry ["check", filename] = loadModuleFile filename >>= putCtx
-- `violet ./sample.vt` will load `sample` into REPL
entry [filename] = loadModuleFile filename >>= \state => new state $ do replLoop
entry xs = primIO $ putDoc $ hsep [
    pretty "unknown command",
    dquotes $ hsep $ map pretty xs
  ]

main : IO ()
main = run $ entry $ drop 1 !getArgs
