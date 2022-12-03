module Violet.Parser

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings
import Violet.Syntax

-- lexeme : Parser a -> Parser a
-- lexeme p = do
--   skip $ many (space <|> newline <|> crlf)
--   p

keyword : String -> Parser ()
keyword s = do
  (lexeme $ string s) <?> "keyword: " ++ s
  pure ()
symbol : String -> Parser ()
symbol s = keyword s <?> s

violetU : Parser Tm
violetU = (keyword "U" <?> "Universe") *>| pure U

violetIdentifier : Parser Name
violetIdentifier = inner <?> "identifier"
  where
   inner = do
     c <- lexeme letter
     cs <- many alphaNum
     pure $ pack (c :: cs)

violetVar : Parser Tm
violetVar = Var <$> violetIdentifier

mutual
  violetTm : Parser Tm
  violetTm = violetLet
    <|> violetPostulate
    <|> violetU
    <|> violetVar
    <|> violetLam
    <|> violetPi
    <|> violetApp

  -- (x : a) → b
  violetPi : Parser Tm
  violetPi = do
    (x, a) <- parens bind
    symbol "->"
    Pi x a <$> violetTm

  violetApp : Parser Tm
  violetApp = ?app

  violetLam : Parser Tm
  violetLam = do
    keyword "λ" <|> keyword "\\"
    xs <- some violetIdentifier
    keyword "."
    t <- violetTm
    pure $ foldr Lam t xs

  violetPostulate : Parser Tm
  violetPostulate = do
    keyword "postulate"
    (x, a) <- bind
    symbol ";"
    Postulate x a <$> violetTm

  violetLet : Parser Tm
  violetLet = do
    keyword "let"
    (x, a) <- bind
    symbol "="
    t <- violetTm
    symbol ";"
    u <- violetTm
    pure $ Let x a t u

  bind : Parser (Name, Tm)
  bind = do
    x <- violetIdentifier
    symbol ":"
    a <- violetTm
    pure (x, a)

export
violetSrc : Parser Tm
violetSrc = violetTm <* eof
