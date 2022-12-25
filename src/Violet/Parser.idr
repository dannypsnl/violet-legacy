module Violet.Parser

import Data.String
import Text.Lexer
import Text.Parser.Core
import Text.Parser
import Text.Parser.Expression

import Violet.Lexer
import Violet.Syntax

public export
Rule : Type -> Type
Rule = Grammar () VToken True

tmU : Rule Raw
tmU = match VTUniverse $> RU

tmVar : Rule Raw
tmVar = RVar <$> match VTIdentifier

parens : Rule a -> Rule a
parens p = match VTOpenP *> p <* match VTCloseP

withPos : (Position -> a -> a) -> Rule a -> Rule a
withPos f p = f (mkPos !location) <$> p

mutual
  atom : Rule Raw
  atom = tmU <|> tmVar <|> (parens tm)

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
    [ Infix (RPi "_" <$ match VTArrow) AssocRight ]
  ] (spine <|> atom)

  tm : Rule Raw
  tm = withPos RSrcPos (tmData <|> tmPostulate <|> tmLet <|> tmLam <|> tmPi <|> expr)

  tmData : Rule Raw
  tmData = do
    match VTData
    name <- match VTIdentifier
    caseList <- many pCase
    match VTSemicolon
    u <- tm
    pure $ RData name caseList u
    where
      pCase : Rule (Name, RTy)
      pCase = do
        match VTVerticalLine
        name <- match VTIdentifier
        match VTColon
        a <- tm
        pure (name, a)

  tmPostulate : Rule Raw
  tmPostulate = do
    match VTPostulate
    name <- match VTIdentifier
    match VTColon
    a <- tm
    match VTSemicolon
    u <- tm
    pure $ RPostulate name a u

  -- λ A x => x
  tmLam : Rule Raw
  tmLam = do
    match VTLambda
    names <- some $ match VTIdentifier
    match VTLambdaArrow
    body <- tm
    pure $ foldr RLam body names

  -- (A : U) -> A -> A
  tmPi : Rule Raw
  tmPi = do
    match VTOpenP
    name <- match VTIdentifier
    match VTColon
    a <- tm
    match VTCloseP
    match VTArrow
    RPi name a <$> tm

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

parseTokens : List (WithBounds VToken) -> Either String Raw
parseTokens toks =
  let toks' = filter (not . ignored) toks
  in case parse tm toks' of
    Right (l, []) => Right l
    Right (_, leftTokens) => Left $ "error: contains tokens that were not consumed\n" ++ show leftTokens
    Left e => Left $ "error:\n" ++ show e ++ "\ntokens:\n" ++ joinBy "\n" (map show toks')
  where
    ignored : WithBounds VToken -> Bool
    ignored (MkBounded (Tok VTIgnore _) _ _) = True
    ignored _ = False

export
parse : String -> Either String Raw
parse str =
  case lexViolet str of
    Just toks => parseTokens toks
    Nothing => Left "error: failed to lex"
