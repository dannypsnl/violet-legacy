module Violet.Parser

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

import Violet.Syntax

withPos : (Position -> a -> a) -> Parser a -> Parser a
withPos f p = f <$> getPosition <*> p

keyword : String -> Parser ()
keyword s = do
  (lexeme $ string s) <?> "keyword: " ++ s
  pure ()
symbol : String -> Parser ()
symbol s = keyword s <?> s
arrowSymbol : Parser ()
arrowSymbol = symbol "->" <|> symbol "→"

violetU : Parser Tm
violetU = (keyword "U" <?> "Universe") *>| pure U

violetIdentifier : Parser Name
violetIdentifier = (pack <$> lexeme letter >! many alphaNum) <?> "identifier"

violetVar : Parser Tm
violetVar = Var <$> violetIdentifier

mutual
  violetTm : Parser Tm
  violetTm = violetLet <|> violetPostulate <|> violetLam <|> violetPi <|> funOrSpine

  violetAtom : Parser Tm
  violetAtom = violetVar <|> violetU <|> parens violetTm

  funOrSpine : Parser Tm
  funOrSpine = do
    sp <- violetSpine
    case !(opt arrowSymbol) of
      Nothing => pure sp
      Just _ => Pi "_" sp <$> violetTm

  -- (x : a) → b
  violetPi : Parser Tm
  violetPi = do
    (x, a) <- parens bind
    arrowSymbol
    Pi x a <$> violetTm

  violetSpine : Parser Tm
  violetSpine = foldl1 App <$> some violetAtom

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
violetSrc = (withPos SrcPos violetTm) <* eof
