import Lean.Data.Parsec
import ParsecExtra
import Lean.Data.Position
import Violet.Ast.Surface

namespace Violet.Parser
open Lean
open Lean.Parsec
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
def identifier : Parsec String := do
  let r ← many1Chars <| satisfy valid?
  if keyword? r then
    fail s!"expected: identifier, got keyword `{r}`"
  whitespace; return r
  where
    valid? : Char → Bool
    | '_' | '?' | '!' => true
    | c => c.isAlphanum
    keyword? : String → Bool
    | "module" | "def" | "data" | "Type" => true
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
    where
      bind : Parsec $ String × Typ := do
        let name : String ← identifier
        keyword ":"; let ty : Typ ← term
        return (name, ty)

  partial def pair : Parsec Tm :=
    parens do
      let l ← term
      keyword ","
      let r ← term
      return .pair l r

  partial def atom : Parsec Tm := do
    parseLam <|> parseLet <|> parseMatch
    <|> kwTyp <|> hole <|> var
    <|> parsePi .implicit braces
    <|> (do
      let r ← tryP <| pair <|> parens term
      match r with
      -- if it's not a parenthesized term, try parsing a pi type
      | .none => parsePi .explicit parens
      | .some s => return s)
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
    |> «mixfixR» [keyword "$" *> return .app .explicit,
                  keyword "<|" *> return .app .explicit]
    |> «mixfix» [keyword "|>" *> return flip (.app .explicit)]
    |> «mixfix» [keyword "->" *> return .pi .explicit "_",
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
        | .implicit => braces
        | .explicit => parens
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

def parseFile : Parsec Program := do
  keyword "module"
  let name ← identifier
  let defs ← many <| parseDef <|> parseData
  eof
  return { name := name, definitions := defs }

end Violet.Parser
