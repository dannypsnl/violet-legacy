module CLI.Check

import Control.App
import Control.App.Handler
import Control.App.Console
import Control.App.FileIO
import Data.String
import System
import System.Path
import Text.PrettyPrint.Prettyprinter.Symbols

import Violet.Elaboration
import Violet.Parser
import Violet.Surface.Syntax

export
record CheckOpts where
  constructor MkCheckOpts
  buildDir : String
  sourceFile : Maybe String

export
defaultCheckOpts : CheckOpts
defaultCheckOpts = MkCheckOpts "build" Nothing

prettyIOError : IOError -> Doc AnsiStyle
prettyIOError err = hsep $ map pretty ["error:", show err]

parseModContent : HasErr PError e => String -> App e ModuleRaw
parseModContent source = do
  Right raw <- pure $ parseViolet ruleModule source
    | Left err => throw err
  pure raw

parseModuleFile
  : (PrimIO e, FileIO (IOError :: e))
  => (filename : String)
  -> App e (ModuleRaw, String)
parseModuleFile filename = do
  primIO $ putStrLn $ "parse module " ++ filename
  source <- readFile filename `handleErr` putErr prettyIOError
  raw <- parseModContent source `handleErr` putErr prettyParsingError
  pure (raw, source)

getImports : (buildDir : String) -> ModuleRaw -> List Name
getImports buildDir raw = map (\(MkModuleImportStmt name) => (moduleNameToFilepath buildDir name)) raw.info.imports
  where
    moduleNameToFilepath : (buildDir : String) -> (modName : Name) -> String
    moduleNameToFilepath buildDir modName = (joinPath $ forget $ cons buildDir $ split (== '.') modName) <.> ".vtt"

checkModContent : Has [PrimIO] e => (filename : String) -> (source : String) -> (ast : ModuleRaw) -> App e CheckState
checkModContent filename source ast = do
  let ctx = (ctxFromFile filename source)
  new (checkState ctx) $ checkModule (map cast ast.tops) `handleErr` putErr prettyCheckError

checkModuleFile
  : (PrimIO e, FileIO (IOError :: e))
  => (config : CheckOpts)
  -> (filename : String)
  -> App e CheckState
checkModuleFile cfg filename = do
  (rawAst, source) <- parseModuleFile filename
  let requiredFiles = getImports cfg.buildDir rawAst
  for_ requiredFiles $ \filename => do
    primIO $ putStrLn $ "require: " ++ show filename
    vtt <- readFile filename `handleErr` putErr (prettyFileRead filename)
    -- TODO: load `.vtt` file to update environment
    pure ()
  checkModContent filename source rawAst
  -- TODO: generate `.vtt` for checked module
  where
    prettyFileRead : (filename : String) -> IOError -> Doc AnsiStyle
    prettyFileRead filename err = hsep $ map pretty [filename ++ ":", show err]


putCtx : PrimIO e => CheckState -> App e ()
putCtx state = do
  let env = MkEnv state.topEnv []
  for_ (reverse state.topCtx.binds) $ \(name, ty) => do
    v <- new state (runQuote env ty) `handleErr` putErr prettyCheckError
    primIO $ putDoc $ (annotate bold $ pretty name)
      <++> ":"
      <++> (annBold $ annColor Blue $ pretty v)

export
checkCommand : (PrimIO e, FileIO (IOError :: e)) => (config : CheckOpts) -> (options : List String) -> App e ()
checkCommand cfg [] = case cfg.sourceFile of
  Just filename => checkModuleFile cfg filename >>= putCtx
  Nothing => primIO $ putDoc $ pretty "missing file, usage `violet check <file>`"
checkCommand cfg ("--build-dir" :: buildDir :: options) = checkCommand ({buildDir := buildDir} cfg) options
checkCommand cfg (filename :: options) = checkCommand ({ sourceFile := Just filename } cfg) options
