module Violet.Lexer

import Text.Lexer
import public Text.Parser.Core
import public Text.Parser

%default total

public export
data VToken
  = Identifier String -- x
  | Let               -- let
  | Postulate         -- postulate
  | Assign            -- =
  | Colon             -- :
  | Semicolon         -- ;
  | OpenP             -- (
  | CloseP            -- )
  | Comment String    -- single line comment or whitespace
  | EOF               -- end of file

||| An identifier starts from alphabet
||| following with alphabet, number, and the below set
||| `-`, `_`, `?`, `!`
export
idChar : Lexer
idChar = pred isIdChar
  where
    isIdChar : Char -> Bool
    isIdChar '-' = True
    isIdChar '_' = True
    isIdChar '?' = True
    isIdChar '!' = True
    isIdChar x = isAlphaNum x

identifier : Lexer
identifier = (pred isAlpha) <+> some idChar

-- 只要還沒碰到換行就是單行的註解內容
comment : Lexer
comment = is '-' <+> is '-' <+> many (isNot '\n')

violetTokens : TokenMap VToken
violetTokens =
  [ (identifier, \x => case x of
      "let" => Let
      "postulate" => Postulate
      x => Identifier x)
  , (is '=', \_ => Assign)
  , (is ':', \_ => Colon)
  , (is ';', \_ => Semicolon)
  , (is '(', \_ => OpenP)
  , (is ')', \_ => CloseP)
  , (spaces, Comment)
  , (comment, Comment)
  ]

export
Show VToken where
  show (Identifier x) = "id " ++ show x
  show Let            = "let"
  show Postulate      = "postulate"
  show Assign         = "="
  show Colon          = ":"
  show Semicolon      = ";"
  show OpenP          = "("
  show CloseP         = ")"
  show (Comment x)    = "comment: " ++ show x
  show EOF            = "<end of file>"

export
Show (TokenData VToken) where
  show (MkToken l c t) = show l ++ ":" ++ show c ++ ": tok=" ++ show t
