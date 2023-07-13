import Lean.Data.Parsec
import ParsecExtra
import Lean.Data.Position
import Violet.Ast.Surface

namespace Violet.Parser
open Lean
open Lean.Parsec
open Violet.Ast
open Violet.Ast.Surface

def whitespace : Parsec Unit := do
  repeat do
    ws
    match ← tryP comment with
    | .none => return ()
    | .some _ => continue
  where
    comment : Parsec Unit := do
      skipString "--"
      while (← peek?).isSome do
        match ← peek! with
        | '\n' => skip; break
        | _ => skip

def keyword (s : String) : Parsec Unit := do
  (skipString s).orElse (fun _ => fail s!"expected: keyword `{s}`")
  whitespace
def integer : Parsec Nat := do
  let s ← many1Chars <| satisfy Char.isDigit
  whitespace
  return s.toNat!
def identifier : Parsec String := do
  let name ← many1Chars <| satisfy valid?
  if keyword? name then
    fail s!"expected: identifier, got keyword `{name}`"
  whitespace; return name
  where
    valid? : Char → Bool
    | '_' | '?' | '!' => true
    | c => c.isAlphanum
    keyword? : String → Bool
    | "module" | "def" | "data" | "record"
    | "Type" => true
    | _ => false

mutual
  partial def parseMatch : Parsec Tm := do
    keyword "match"; let target ← term
    let cs ← many clause
    return .«match» target cs
    where
      pattern : Parsec Pattern := do
        let ctor ← identifier
        let vars ← many identifier
        return ⟨ ctor, vars ⟩
      clause : Parsec $ Pattern × Tm := do
        keyword "|"; let pat ← pattern
        keyword "=>"; let tm ← term
        return ⟨ pat, tm ⟩

  -- `let <name> : <ty> = <val>; <body>`
  partial def parseLet : Parsec Tm := do
    keyword "let"; let name ← identifier
    keyword ":"; let ty ← term
    keyword "="; let val ← term
    keyword ";"; let body ← term
    return .«let» name ty val body

  partial def parseLam : Parsec Tm := do
    keyword "λ" <|> keyword "\\"
    let names ← many1 identifier
    keyword "=>"
    let body ← term
    return  names.foldr (.lam .explicit) body

  partial def parsePi (mode : Mode) (w : Parsec (String × Typ) → Parsec (String × Typ))
    : Parsec Tm := do
    let (name, ty) ← w bind
    keyword "->" <|> keyword "→"
    let body ← term
    return .pi mode name ty body

  partial def bind : Parsec $ String × Typ := do
    let name : String ← identifier
    keyword ":"; let ty : Typ ← term
    return (name, ty)

  partial def parseSigma : Parsec Tm := do
    let (name, ty) ← (parens (user_ws := whitespace)) bind
    keyword "**" <|> keyword "×"
    let body ← term
    return .sigma name ty body

  partial def pair : Parsec Tm :=
    (parens (user_ws := whitespace)) do
      let l ← term
      keyword ","
      let r ← term
      return .pair l r

  partial def atom : Parsec Tm := do
    parseLam <|> parseLet <|> parseMatch
    <|> kwTyp <|> hole <|> var
    <|> parsePi .implicit (braces (user_ws := whitespace))
    <|> (do
      let r ← tryP <| pair
      if r.isSome then return r.get!
      let r ← tryP <| (parens (user_ws := whitespace)) term
      if r.isSome then return r.get!
      let r ← tryP parseSigma
      if r.isSome then return r.get!
      parsePi .explicit (parens (user_ws := whitespace)))
    where
      hole : Parsec Tm := do
        keyword "!!"
        return Tm.hole
      var : Parsec Tm := identifier
      kwTyp := do keyword "Type"; return Tm.type

  partial def spine : Parsec Tm := do
    let mut l ← atom
    repeat do
      match ← tryP atom with
      | .none => break
      | .some r => l := .app .explicit l r
    return l

  partial def term : Parsec Tm :=
    spine
    |> «postfix» [keyword "." *> .proj <$> integer]
    |> «infixL» [keyword "×" *> return .sigma "_"]
    |> «infixR» [keyword "$" *> return .app .explicit,
                  keyword "<|" *> return .app .explicit]
    |> «infixL» [keyword "|>" *> return flip (.app .explicit)]
    |> «infixL» [keyword "->" *> return .pi .explicit "_",
                 keyword "→" *> return .pi .explicit "_"]
    |> λ p => withPosition p .src
end

abbrev typ := term

def telescope : Parsec Telescope := do
  let grps ← many $ bindGroup .explicit <|> bindGroup .implicit
  return grps.toList.join |> List.toArray
  where
    bindGroup (mode : Mode) :=
      let wrapper := match mode with
        | .implicit => braces (user_ws := whitespace)
        | .explicit => parens (user_ws := whitespace)
      wrapper $ do
        let names ← many1 identifier
        keyword ":"
        let ty ← typ
        return Array.toList <| names.map fun name => (name, mode, ty)
def parseDef : Parsec Definition := do
  let startPos ← getPosition
  keyword "def"
  let name ← identifier
  let tele ← telescope
  keyword ":"
  let ret_ty ← typ
  -- TODO: or pattern matching clauses
  let body ← singleBody
  let endPos ← getPosition
  return .«def» startPos endPos name tele ret_ty body
  where
    singleBody : Parsec Tm := do keyword "=>"; term

def parseData : Parsec Definition := do
  let startPos ← getPosition
  keyword "data"
  let name ← identifier
  let cs ← many constructor
  let endPos ← getPosition
  return .data startPos endPos name cs
  where
    constructor : Parsec Ctor := do
      keyword "|"
      let name ← identifier
      let mut tys : Array Typ := #[]
      repeat do
        match ← tryP typ with
        | .none => break
        | .some ty => tys := tys.push ty
      return ⟨ name, tys ⟩

def parseRecord : Parsec Definition := do
  let startPos ← getPosition
  keyword "record"
  let name ← identifier
  keyword "where"
  let mut fs := #[]

  repeat do
    match ← tryP field with
    | .none => break
    | .some f => fs := fs.push f

  let endPos ← getPosition
  return .record startPos endPos name fs
  where
    field : (Parsec <| String × Typ) := do
      let name ← identifier
      keyword ":"
      let ty ← typ
      keyword ";"
      return ⟨ name, ty ⟩

def parseFile : Parsec Program := do
  keyword "module"
  let name ← identifier
  let defs ← many <| parseDef <|> parseData <|> parseRecord
  eof
  return { name := name, definitions := defs }

end Violet.Parser
