module Violet.Parser

import Text.Lexer
import public Text.Parser.Core
import public Text.Parser

import Violet.Lexer
import Violet.Syntax

tmU : Grammar state VToken True Tm
tmU = match Universe $> U

tmVar : Grammar state VToken True Tm
tmVar = Var <$> match Identifier

parens : Grammar state VToken True a -> Grammar state VToken True a
parens p = match OpenP *> p <* match CloseP

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
    match Postulate
    name <- match Identifier
    match Colon
    a <- tm
    match Semicolon
    u <- tm
    pure $ Postulate name a u

  -- λ A x . x
  tmLam : Grammar state VToken True Tm
  tmLam = do
    match Lambda
    names <- some $ match Identifier
    match Dot
    body <- tm
    pure $ foldr Lam body names

  -- (A : U) -> A -> A
  tmPi : Grammar state VToken True Tm
  tmPi = do
    match OpenP
    name <- match Identifier
    match Colon
    a <- tm
    match CloseP
    match Arrow
    Pi name a <$> tm

  -- let x : a = t; u
  tmLet : Grammar state VToken True Tm
  tmLet = do
    match Let
    name <- match Identifier
    match Colon
    a <- tm
    match Assign
    t <- tm
    match Semicolon
    u <- tm
    pure $ Let name a t u

tmFull : Grammar state VToken True Tm
tmFull = tm

export
parse : String -> Either String Tm
parse str =
  case lexViolet str of
    Just toks => pp toks
    Nothing => Left "error: failed to lex."
  where
    pp : List (WithBounds VToken) -> Either String Tm
    pp toks =
      case parse tmFull toks of
        Right (l, []) => Right l
        Right e => Left "contains tokens that were not consumed"
        Left e => Left (show e)
