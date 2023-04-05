module Violet.Parser

import Data.List
import Text.Parser.Core
import Text.Parser
import Text.Parser.Expression

import Violet.Surface.Lexer
import public Violet.Surface.Syntax
import public Violet.Surface.Error

-- `Grammar state tok consumes ty`
-- * `state` can be defined, but didn't be used here
-- * `tok` is the token type of the language
-- * `consumes` flag is True if the language is guaranteed to consume some input on success
-- * `ty` is the returned type of parser
public export
Rule : (ty : Type) -> Type
Rule = Grammar () VToken True

tmU : Rule Raw
tmU = match VTUniverse $> RU

tmVar : Rule Raw
tmVar = RVar <$> match VTIdentifier

tmHole : Rule Raw
tmHole = do
  match VTQuestionMark
  RHole <$> match VTIdentifier

parens : Rule a -> Rule a
parens p = match VTOpenP *> p <* match VTCloseP
braces : Rule a -> Rule a
braces p = match VTOpenB *> p <* match VTCloseB

mutual
  atom : Rule Raw
  atom = tmHole <|> tmU <|> tmVar <|> (parens tm)

  spine : Rule Raw
  spine = foldl1 RApp <$> some atom

  expr : Rule Raw
  expr = buildExpressionParser [
    -- dollar sign can use to avoid parentheses
    -- the following expressions are same
    -- `a $ b c d` <=> `a (b c d)`
    -- except the arrow symbol, it's the lowest
    -- since it's about to reorganize the code
    [ Infix (RApp <$ match VTDollar) AssocRight ],
    -- To be convience, thus, arrow is the lowsest symbol
    -- For example, user might write `List a -> List b`, here `List a` is an application
    [ Infix (RPi Explicit "_" <$ match VTArrow) AssocRight ]
  ] (spine <|> atom)

  tm : Rule Raw
  tm = do
    r <- bounds (tmLet <|> tmElim <|> tmLam <|> tmImplicitPi <|> tmPi <|> expr)
    pure $ RSrcPos r

  -- λ A x => x
  tmLam : Rule Raw
  tmLam = do
    match VTLambda
    names <- some $ match VTIdentifier
    match VTLambdaArrow
    body <- tm
    pure $ foldr RLam body names

  -- (a : A) -> b
  tmPi : Rule Raw
  tmPi = do
    match VTOpenP
    name <- match VTIdentifier
    match VTColon
    a <- tm
    match VTCloseP
    match VTArrow
    RPi Explicit name a <$> tm

  -- {a : A} -> b
  tmImplicitPi : Rule Raw
  tmImplicitPi = do
    match VTOpenB
    name <- match VTIdentifier
    match VTColon
    a <- tm
    match VTCloseB
    match VTArrow
    RPi Implicit name a <$> tm

  -- let x : a = t; u
  tmLet : Rule Raw
  tmLet = do
    match VTLet
    name <- match VTIdentifier
    match VTColon
    a <- tm
    match VTAssign
    t <- tm
    match VTSemicolon
    u <- tm
    pure $ RLet name a t u

  patRule : Rule Pat
  patRule = some (match VTIdentifier)
  patsRule : Rule (List Pat)
  patsRule = do
    match VTVerticalLine
    sepBy (match VTComma) patRule
  caseRule : Rule (List ElimCase)
  caseRule = do
    pss <- some patsRule
    match VTLambdaArrow
    t <- tm
    pure $ map (\ps => (ps, t)) $ forget pss
  -- The whole will look like
  --
  -- elim n
  -- | C x => x
  -- | z => z
  tmElim : Rule Raw
  tmElim = do
    -- `elim n`
    match VTElim
    targets <- sepBy (match VTComma) tm
    -- ```
    -- | C x => x
    -- | z => z
    -- ```
    let cs : List (List ElimCase) = !(many caseRule)
    pure $ RElim targets (foldl (\c, cs => cs ++ c) [] cs)

ttmData : Rule TopLevelRaw
ttmData = do
  match VTData
  name <- match VTIdentifier
  caseList <- many pCase
  pure $ TData name [] caseList
  where
    pCase : Rule (Name, List RTy)
    pCase = do
      match VTVerticalLine
      name <- match VTIdentifier
      a <- many tm
      pure (name, a)

-- def x (xi : Ti) : T =>
--   u
ttmDef : Rule TopLevelRaw
ttmDef = do
  match VTDef
  name <- match VTIdentifier
  raw_tele <- many $ try (parens (bindGroup Explicit)) <|> (braces (bindGroup Implicit))
  let regular_tele : RTelescope = foldl mergeTele [] raw_tele
  match VTColon
  -- returned type
  a <- tm
  t <- singleBody <|> (do pure $ RElim (extractTargets regular_tele) (foldl (\c, cs => cs ++ c) [] !(many caseRule)))
  pure $ TDef name regular_tele a t
  where
    ignoreImplicit : (Mode, Name, RTy) -> List Raw -> List Raw
    ignoreImplicit (Implicit, _, _) acc = acc
    ignoreImplicit (Explicit, name, _) acc = RVar name :: acc
    -- skip implicit by default
    extractTargets : RTelescope -> List Raw
    extractTargets tele = foldr ignoreImplicit [] tele

    mergeTele : RTelescope -> RTelescope -> RTelescope
    mergeTele x tele = x ++ tele

    singleBody : Rule Raw
    singleBody = do
      match VTLambdaArrow
      tm

    bindGroup : Mode -> Rule (List (Mode, Name, RTy))
    bindGroup mode = do
      ns <- some $ match VTIdentifier
      match VTColon
      ty <- tm
      pure $ map (\n => (mode, n, ty)) $ forget ns

ttm : Rule TopLevelRaw
ttm = TSrcPos <$> bounds (ttmData <|> ttmDef)

export
ruleTm : Rule Raw
ruleTm = tm

ruleModuleImport : Rule ModuleImportStmt
ruleModuleImport = do
  match VTImport
  name <- match VTIdentifier
  pure $ MkModuleImportStmt name

export
ruleModule : Rule ModuleRaw
ruleModule = do
  match VTModule
  name <- match VTIdentifier
  imports <- many ruleModuleImport
  bindings <- many ttm
  pure $ MkModuleRaw (MkModuleInfoRaw name imports) bindings

export
parseViolet : Rule a -> String -> Either PError a
parseViolet rule source =
  case lexViolet source of
    Just toks => parseTokens toks source
    Nothing => Left LexFail
  where
    parseTokens : List (WithBounds VToken) -> (source : String) -> Either PError a
    parseTokens toks source =
      let toks' = filter (not . ignored) toks
      in case parse rule toks' of
        Right (l, []) => Right l
        Right (_, leftTokens) => Left $ TokensLeave source leftTokens
        Left es => Left $ CollectPError $ map (\(Error msg bounds) => SinglePError source bounds msg) $ forget es
      where
        ignored : WithBounds VToken -> Bool
        ignored (MkBounded (Tok VTIgnore _) _ _) = True
        ignored _ = False
