module Violet.Parser

import Data.String
import Text.Lexer
import public Text.Parser.Core
import public Text.Parser

import Violet.Lexer
import Violet.Syntax
import Violet.Core.Position

public export
Rule : Type -> Type
Rule = Grammar () VToken True

tmU : Rule Raw
tmU = match VTUniverse $> RU
 
tmVar : Rule Raw
tmVar = RVar <$> match VTIdentifier
 
parens : Rule a -> Rule a
parens p = match VTOpenP *> p <* match VTCloseP

mutual 
  atom : Rule Raw
  atom = tmU <|> tmVar <|> (parens tm)
 
  spine : Rule Raw
  spine = foldl1 RApp <$> some atom

  -- a -> b -> c
  --
  -- or
  --
  -- a b c 
  funOrSpine : Rule Raw
  funOrSpine = do
    sp <- spine
    option sp (RPi "_" sp <$> tm)
 
  tm : Rule Raw
  tm = tmData <|> tmPostulate <|> tmLet <|> tmLam <|> tmPi <|> spine
 
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

  -- λ A x . x 
  tmLam : Rule Raw
  tmLam = do
    match VTLambda
    names <- some $ match VTIdentifier
    match VTDot
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

srcTm : Rule Raw
srcTm = do
  loc <- location
  RSrcPos (mkPos loc) <$> tm

parseTokens : List (WithBounds VToken) -> Either String Raw
parseTokens toks =
  let toks' = filter (not . ignored) toks
  in case parse srcTm toks' of
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
