module Violet.Parser

import Data.String
import Text.Lexer
import public Text.Parser.Core
import public Text.Parser

import Violet.Lexer
import Violet.Syntax

tmU : Grammar state VToken True Tm
tmU = match VTUniverse $> U

tmVar : Grammar state VToken True Tm
tmVar = Var <$> match VTIdentifier

parens : Grammar state VToken True a -> Grammar state VToken True a
parens p = match VTOpenP *> p <* match VTCloseP

mutual
  atom : Grammar state VToken True Tm
  atom = tmU <|> tmVar <|> (parens tm)

  spine : Grammar state VToken True Tm
  spine = foldl1 App <$> some atom

  -- a -> b -> c
  --
  -- or
  --
  -- a b c
  funOrSpine : Grammar state VToken True Tm
  funOrSpine = do
    sp <- spine
    option sp (Pi "_" sp <$> tm)

  tm : Grammar state VToken True Tm
  tm = tmPostulate <|> tmLet <|> tmLam <|> tmPi <|> funOrSpine

  tmPostulate : Grammar state VToken True Tm
  tmPostulate = do
    match VTPostulate
    name <- match VTIdentifier
    match VTColon
    a <- tm
    match VTSemicolon
    u <- tm
    pure $ Postulate name a u

  -- λ A x . x
  tmLam : Grammar state VToken True Tm
  tmLam = do
    match VTLambda
    names <- some $ match VTIdentifier
    match VTDot
    body <- tm
    pure $ foldr Lam body names

  -- (A : U) -> A -> A
  tmPi : Grammar state VToken True Tm
  tmPi = do
    match VTOpenP
    name <- match VTIdentifier
    match VTColon
    a <- tm
    match VTCloseP
    match VTArrow
    Pi name a <$> tm

  -- let x : a = t; u
  tmLet : Grammar state VToken True Tm
  tmLet = do
    match VTLet
    name <- match VTIdentifier
    match VTColon
    a <- tm
    match VTAssign
    t <- tm
    match VTSemicolon
    u <- tm
    pure $ Let name a t u

export
parse : String -> Either String Tm
parse str =
  case lexViolet str of
    Just toks => parseTokens toks
    Nothing => Left "error: failed to lex."
  where
    ignored : WithBounds VToken -> Bool
    ignored (MkBounded (Tok VTIgnore _) _ _) = True
    ignored _ = False
    parseTokens : List (WithBounds VToken) -> Either String Tm
    parseTokens toks =
      let toks' = filter (not . ignored) toks
      in case parse tm toks' of
        Right (l, []) => Right l
        Right e => Left "error: contains tokens that were not consumed"
        Left e => Left $ "error:\n" ++ show e ++ "\ntokens:\n" ++ joinBy "\n" (map show toks')
