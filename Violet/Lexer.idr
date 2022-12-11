module Violet.Lexer

import Text.Lexer
import Text.Token
import Data.List

%default total

public export
data VTokenKind
  = Identifier        -- x
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
  | Comment           -- single line comment or whitespace

export
Eq VTokenKind where
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
  _ == _ = False

export
Show VTokenKind where
  show Identifier = "<identifer>"
  show Let        = "let"
  show Postulate  = "postulate"
  show Universe   = "U"
  show Assign     = "="
  show Colon      = ":"
  show Semicolon  = ";"
  show OpenP      = "("
  show CloseP     = ")"
  show Arrow      = "→"
  show Lambda     = "λ"
  show Dot        = "."
  show Comment    = "<comment>"

public export
VToken : Type
VToken = Token VTokenKind

export
Show VToken where
  show (Tok kind text) = "Tok kind: " ++ show kind ++ " text: " ++ text

export
TokenKind VTokenKind where
  TokType Identifier = String
  TokType _ = ()

  tokValue Identifier s = s
  tokValue Let _ = ()
  tokValue Postulate _ = ()
  tokValue Universe _ = ()
  tokValue Assign _ = ()
  tokValue Colon _ = ()
  tokValue Semicolon _ = ()
  tokValue OpenP _ = ()
  tokValue CloseP _ = ()
  tokValue Arrow _ = ()
  tokValue Lambda _ = ()
  tokValue Dot _ = ()
  tokValue Comment _ = ()

ignored : WithBounds VToken -> Bool
ignored (MkBounded (Tok Comment _) _ _) = True
ignored _ = False

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
-- comment : Lexer
-- comment = is '-' <+> is '-' <+> many (isNot '\n')

keywords : List (String, VTokenKind)
keywords = [
  ("let", Let),
  ("postulate", Postulate),
  ("U", Universe)
]

export
violetTokens : TokenMap VToken
violetTokens = toTokenMap [(spaces, Comment)] ++
  [(identifier, \s =>
      case lookup s keywords of
        (Just kind) => Tok kind s
        Nothing => Tok Identifier s
    )
  ] ++ toTokenMap [
    (exact ":", Colon),
    (exact ";", Semicolon),
    (exact "->", Arrow),
    (exact "\\", Lambda),
    (exact ".", Dot),
    (exact "(", OpenP),
    (exact ")", CloseP),
    (exact "=", Assign)
  ]

lexViolet : String -> Maybe (List (WithBounds VToken))
lexViolet str =
  case lex violetTokens str of
    (tokens, _, _, "") => Just (filter (not . ignored) tokens)
    _ => Nothing
