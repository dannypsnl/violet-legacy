module Violet.Parser

import Text.Lexer
import public Text.Parser.Core
import public Text.Parser

import Violet.Lexer
import Violet.Syntax

public export
Rule : Type -> Type
Rule ty = Grammar (TokenData VToken) True ty

eat : VToken -> Rule ()
eat t = terminal (\x => if (t == tok x) then Just () else Nothing)
keyword : VToken -> Rule ()
keyword = eat
symbol : VToken -> Rule ()
symbol = eat
eoi : Rule ()
eoi = eat EndInput

identifier : Rule Name
identifier = terminal (\x => case tok x of
  Identifier x => Just x
  _ => Nothing)

tmU : Rule Tm
tmU = terminal (\x => case tok x of
  Universe => Just U
  _ => Nothing)

tmVar : Rule Tm
tmVar = terminal (\x => case tok x of
  Identifier x => Just $ Var x
  _ => Nothing)

parens : Rule a -> Rule a
parens p = symbol OpenP *> p <* symbol CloseP

mutual
  atom : Rule Tm
  atom = tmU <|> tmVar <|> (parens tm)

  spine : Rule Tm
  spine = foldl1 App <$> some atom

  -- a -> b -> c
  --
  -- or
  --
  -- a b c
  funOrSpine : Rule Tm
  funOrSpine = do
    sp <- spine
    option sp (Pi "_" sp <$> tm)

  tm : Rule Tm
  tm = tmPostulate <|> tmLet <|> tmLam <|> tmPi <|> funOrSpine

  tmPostulate : Rule Tm
  tmPostulate = do
    keyword Postulate
    name <- identifier
    symbol Colon
    a <- tm
    symbol Semicolon
    u <- tm
    pure $ Postulate name a u

  -- λ A x . x
  tmLam : Rule Tm
  tmLam = do
    keyword Lambda
    names <- some identifier
    symbol Dot
    body <- tm
    pure $ foldr Lam body names

  -- (A : U) -> A -> A
  tmPi : Rule Tm
  tmPi = do
    symbol OpenP
    name <- identifier
    symbol Colon
    a <- tm
    symbol CloseP
    symbol Arrow
    Pi name a <$> tm

  -- let x : a = t; u
  tmLet : Rule Tm
  tmLet = do
    keyword Let
    name <- identifier
    symbol Colon
    a <- tm
    symbol Assign
    t <- tm
    symbol Semicolon
    u <- tm
    pure $ Let name a t u

export
tmFull : Rule Tm
tmFull = tm <* eoi
