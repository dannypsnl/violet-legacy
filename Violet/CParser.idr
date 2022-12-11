module Violet.CParser

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

mutual
  atom : Rule Tm
  atom = tmU <|> tmVar -- <|> (parens tm)

  funOrSpine : Rule Tm
  funOrSpine = ?todo

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

  tmLam : Rule Tm
  tmLam = do
    keyword Lambda
    names <- some identifier
    symbol Dot
    body <- tm
    pure $ foldr Lam body names

  tmPi : Rule Tm
  tmPi = ?todo

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
