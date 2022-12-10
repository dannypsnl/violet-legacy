module Violet.Lexer

import Text.Lexer

%default total

public export
data VToken
  = Identifier String -- x
  | Let               -- let
  | Postulate         -- postulate
  | Universe          -- U
  | Assign            -- =
  | Colon             -- :
  | Semicolon         -- ;
  | OpenP             -- (
  | CloseP            -- )
  | Arrow             -- →
  | Lambda            -- λ
  | Dot               -- .
  | Comment String    -- single line comment or whitespace
  | EndInput          -- end of input

||| An identifier starts from alphabet
||| following with alphabet, number, and the below set
||| `-`, `_`, `?`, `!`
isIdChar : Char -> Bool
isIdChar '-' = True
isIdChar '_' = True
isIdChar '?' = True
isIdChar '!' = True
isIdChar x = isAlphaNum x

identifier : Lexer
identifier = (pred isAlpha) <+> many (pred isIdChar)

-- 只要還沒碰到換行就是單行的註解內容
comment : Lexer
comment = is '-' <+> is '-' <+> many (isNot '\n')

export
violetTokens : TokenMap VToken
violetTokens =
  [ (spaces, Comment)
  , (comment, Comment)
  , (is '=', \_ => Assign)
  , (is ':', \_ => Colon)
  , (is ';', \_ => Semicolon)
  , (is '(', \_ => OpenP)
  , (is ')', \_ => CloseP)
  , (is '→', \_ => Arrow)
  , (is 'λ', \_ => Lambda)
  , (is '.', \_ => Dot)
  , (identifier, \x => case x of
      "let" => Let
      "postulate" => Postulate
      "U" => Universe
      x => Identifier x)
  ]

export
skipWs : (List (TokenData VToken), Int, Int, String) -> (List (TokenData VToken), Int, Int, String)
skipWs (ts, l, c, s) = ((filter notComment ts) ++ [MkToken l c EndInput], l, c, s)
  where
    notComment : TokenData VToken -> Bool
    notComment t = case (tok t) of
      (Comment _) => False
      _ => True

export
Eq VToken where
  Let == Let = True
  Postulate == Postulate = True
  Universe == Universe = True
  Assign == Assign = True
  Colon == Colon = True
  Semicolon == Semicolon = True
  OpenP == OpenP = True
  CloseP == CloseP = True
  Arrow == Arrow = True
  Lambda == Lambda = True
  Dot == Dot = True
  EndInput == EndInput = True
  _ == _ = False

export
Show VToken where
  show (Identifier x) = "id " ++ show x
  show Let            = "let"
  show Postulate      = "postulate"
  show Universe       = "U"
  show Assign         = "="
  show Colon          = ":"
  show Semicolon      = ";"
  show OpenP          = "("
  show CloseP         = ")"
  show Arrow          = "→"
  show Lambda         = "λ"
  show Dot            = "."
  show (Comment x)    = "comment: " ++ show x
  show EndInput       = "<end of input>"

export
Show (TokenData VToken) where
  show (MkToken l c t) = show l ++ ":" ++ show c ++ ": tok=" ++ show t
