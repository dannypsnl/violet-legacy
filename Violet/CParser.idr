module Violet.CParser

import Text.Lexer
import public Text.Parser.Core
import public Text.Parser

import Violet.Lexer
import Violet.Syntax

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
  tm : Rule Tm
  tm = tmLet <|> tmU <|> tmVar

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

tmFull : Rule Tm
tmFull = tm <* eoi

export
test : String -> Either (ParseError (TokenData VToken)) (Tm, List (TokenData VToken))
test s = parse tmFull $ fst (skipWs (lex violetTokens s))
