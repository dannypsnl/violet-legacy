module Violet.Lexer

import Text.Lexer
import Text.Token
import Data.List

%default total

public export
data VTokenKind
  = VTIdentifier        -- x
  | VTLet               -- let
  | VTPostulate         -- postulate
  | VTUniverse          -- U
  | VTAssign            -- =
  | VTColon             -- :
  | VTSemicolon         -- ;
  | VTOpenP             -- (
  | VTCloseP            -- )
  | VTArrow             -- →
  | VTLambda            -- λ
  | VTDot               -- .
  | VTIgnore            -- single line comment or whitespace

export
Eq VTokenKind where
  (==) VTIdentifier VTIdentifier = True
  (==) VTLet VTLet = True
  (==) VTPostulate VTPostulate = True
  (==) VTUniverse VTUniverse = True
  (==) VTAssign VTAssign = True
  (==) VTColon VTColon = True
  (==) VTSemicolon VTSemicolon = True
  (==) VTOpenP VTOpenP = True
  (==) VTCloseP VTCloseP = True
  (==) VTArrow VTArrow = True
  (==) VTLambda VTLambda = True
  (==) VTDot VTDot = True
  (==) _ _ = False

export
Show VTokenKind where
  show VTIdentifier = "<identifer>"
  show VTLet        = "let"
  show VTPostulate  = "postulate"
  show VTUniverse   = "U"
  show VTAssign     = "="
  show VTColon      = ":"
  show VTSemicolon  = ";"
  show VTOpenP      = "("
  show VTCloseP     = ")"
  show VTArrow      = "→"
  show VTLambda     = "λ"
  show VTDot        = "."
  show VTIgnore     = "<ignore>"

public export
VToken : Type
VToken = Token VTokenKind

export
Show VToken where
  show (Tok kind text) = "Tok kind: " ++ show kind ++ "\n text: " ++ text

export
TokenKind VTokenKind where
  TokType VTIdentifier = String
  TokType _ = ()

  tokValue VTIdentifier s = s
  tokValue VTLet _ = ()
  tokValue VTPostulate _ = ()
  tokValue VTUniverse _ = ()
  tokValue VTAssign _ = ()
  tokValue VTColon _ = ()
  tokValue VTSemicolon _ = ()
  tokValue VTOpenP _ = ()
  tokValue VTCloseP _ = ()
  tokValue VTArrow _ = ()
  tokValue VTLambda _ = ()
  tokValue VTDot _ = ()
  tokValue VTIgnore _ = ()

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

keywords : List (String, VTokenKind)
keywords = [
  ("let", VTLet),
  ("postulate", VTPostulate),
  ("U", VTUniverse)
]

violetTokenMap : TokenMap VToken
violetTokenMap = toTokenMap [
    (spaces, VTIgnore),
    (comment, VTIgnore),
    (exact ":", VTColon),
    (exact ";", VTSemicolon),
    (exact "→", VTArrow),
    (exact "λ", VTLambda),
    (exact ".", VTDot),
    (exact "(", VTOpenP),
    (exact ")", VTCloseP),
    (exact "=", VTAssign)
  ] ++
  [ (identifier, \s =>
      case lookup s keywords of
        (Just kind) => Tok kind s
        Nothing => Tok VTIdentifier s
    )
  ]

export
lexViolet : String -> Maybe (List (WithBounds VToken))
lexViolet str =
  case lex violetTokenMap str of
    (tokens, _, _, "") => Just tokens
    _ => Nothing
