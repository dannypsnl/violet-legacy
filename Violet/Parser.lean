import Lean.Data.Parsec
import Violet.Ast.Surface

namespace Lean.Parsec
open Parsec.ParseResult

def getSourcePos : Parsec String.Pos := fun it : String.Iterator =>
  success it it.pos

-- The roll back is done by using old `it`
def tryP (p : Parsec a) : Parsec (Option a) := λ it =>
  match p it with
  | .success rem a => .success rem a
  | .error _ _ => .success it .none

def between (before : Parsec Unit) (p : Parsec a) (after : Parsec Unit) := do
  before; let r ← p; after; return r

def parens (p : Parsec a) : Parsec a := between (skipChar '(') p (skipChar ')')
def braces (p : Parsec a) : Parsec a := between (skipChar '{') p (skipChar '}')

end Lean.Parsec

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
    | "def" | "data" | "Type" => true
    | _ => false

def term : Parsec Tm := kwTyp <|> var
  where
    var : Parsec Tm := identifier
    kwTyp := do keyword "Type"; return Tm.type
abbrev typ := term

def telescope : Parsec Telescope := do
  let grps ← many $
    bindGroup .explicit <* ws
    <|> bindGroup .implicit <* ws
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
  let body ← singleBody
  return .«def» name tele ret_ty body
  where
    singleBody : Parsec Tm := do keyword "=>"; term

def parseData : Parsec Definition := do
  keyword "data"
  let name ← identifier
  let ctors ← many constructor
  return .data name ctors
  where
    constructor : Parsec Constructor := do
      keyword "|"
      let name ← identifier
      let mut tys : Array Typ := #[]
      let mut ty ← tryP typ
      while ty.isSome do
        tys := tys.push ty.get!
        ty ← tryP typ
      return ⟨ name, tys ⟩

def parseFile : Parsec Program := do
  let defs ← many $ parseDef <|> parseData
  eof
  return { definitions := defs }

end Violet.Parser
