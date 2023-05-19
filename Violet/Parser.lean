import Lean.Data.Parsec
import ParsecExtra
import Lean.Data.Position
import Violet.Ast.Surface

namespace Violet.Parser
open Lean
open Lean.Parsec
open Violet.Ast.Surface

def keyword (s : String) := do
  (skipString s).orElse (fun _ => fail s!"expected: keyword `{s}`")
  ws
def identifier := do
  let r ← many1Chars <| satisfy valid?
  if keyword? r then
    fail s!"expected: identifier, got keyword `{r}`"
  ws; return r
  where
    valid? : Char → Bool
    | '-' | '_' | '?' | '!' => true
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
    return  names.foldr .lam body

  partial def parsePi (mode : Mode) (w : Parsec (String × Typ) → Parsec (String × Typ))
    : Parsec Tm := do
    let (name, ty) ← w bind
    ws
    keyword "->"; let body ← term
    return .pi mode name ty body
    where
      bind : Parsec $ String × Typ := do
        let name : String ← identifier
        keyword ":"; let ty : Typ ← term
        return (name, ty)

  partial def atom : Parsec Tm := do
    parseLam <|> parseLet <|> parseMatch
    <|> kwTyp <|> var
    <|> parsePi .implicit braces
    <|> (do
      let r ← tryP $ parens term <* ws
      match r with
      -- if it's not a parenthesized term, try parsing a pi type
      | .none => parsePi .explicit parens
      | .some s => return s)
    where
      var : Parsec Tm := identifier
      kwTyp := do keyword "Type"; return Tm.type

  partial def spine : Parsec Tm := do
    let mut l ← atom
    repeat do
      match ← tryP atom with
      | .none => break
      | .some r => l := .app l r
    return l

  partial def term : Parsec Tm :=
    spine
    |> «mixfix» [keyword "$" *> return .app,
                 keyword "<|" *> return .app,
                 keyword "|>" *> return flip .app]
    |> «mixfix» [keyword "->" *> return .pi .explicit "_",
                 keyword "→" *> return .pi .explicit "_"]
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
  keyword "def"
  let name ← identifier
  let tele ← telescope
  keyword ":"
  let ret_ty ← typ
  -- TODO: or pattern matching clauses
  let body ← singleBody
  return .«def» name tele ret_ty body
  where
    singleBody : Parsec Tm := do keyword "=>"; term

def parseData : Parsec Definition := do
  keyword "data"
  let name ← identifier
  return .data name (← many constructor)
  where
    constructor : Parsec Constructor := do
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
  let defs ← many $ parseDef <|> parseData
  -- eof
  return { name := name, definitions := defs }

end Violet.Parser
