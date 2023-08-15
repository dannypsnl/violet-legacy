import LSpec
import Violet.Parser

open LSpec
open Violet.Parser Violet.Ast.Surface

def dummyPos : Lean.Position := { line := 0, column := 0 }

-- `src` is recording start and end `Position` information, but bad for testing
-- so here convert them to string will remove position's difference
instance : BEq Tm where
  beq x y := x.toString == y.toString

instance : BEq Definition where
  beq x y :=
    match x, y with
    | (.def _ _ name tele ret_ty body), (.def _ _ name' tele' ret_ty' body') =>
      name == name' && tele == tele' && ret_ty == ret_ty' && body == body'
    | (.data _ _ name ctors), (.data _ _ name' ctors') =>
      name == name' && ctors == ctors'
    | (.record _ _ name fields), (.record _ _ name' fields') =>
      name == name' && fields == fields'
    | _, _ => false

instance [BEq α] : BEq (Except ε α) where
  beq | .ok x, .ok y => x == y
      | _, _ => false

def testPiType :=
  lspecIO $ group "pi type"
  $ test "explicit"
    (term.run "(a : Type) → Type" == .ok (.pi .explicit "a" .type .type))
  $ test "implicit"
    (term.run "{a : Type} → Nat" == .ok (.pi .implicit "a" .type (.var "Nat")))
  $ test "non dependent"
    (term.run "a → b" == .ok (.pi .explicit "_" (.var "a") (.var "b")))
  $ test "test non-unicode"
    (term.run "(a : Type) -> Type" |> Except.isOk)

def testApp :=
  lspecIO $ group "application"
  $ test "$ operator"
    (term.run "a $ b c" == .ok (.app .explicit (.var "a") (.app .explicit (.var "b") (.var "c"))))
  $ test "<| operator"
    (term.run "a <| b <| c" == .ok (.app .explicit (.var "a")
      (.app .explicit (.var "b") (.var "c"))))
  $ test "|> operator"
    (term.run "d |> b c |> a" == .ok (
      .app .explicit (.var "a")
        (.app .explicit
          (.app .explicit (.var "b") (.var "c"))
          (.var "d"))))

def testPair :=
  lspecIO $ group "pair"
  $ test "base case"
    (term.run "(a, b)" == .ok (.pair (.var "a") (.var "b")))

def testSigmaType :=
  lspecIO $ group "sigma type"
  $ test "base case"
    (term.run "(x : A) × B" == .ok (.sigma "x" (.var "A") (.var "B")))
  $ test "base case 2"
    (term.run "(x : A) ** B" == .ok (.sigma "x" (.var "A") (.var "B")))
  $ test "product type abbreviation"
    (term.run "A × B" == .ok (.sigma "_" (.var "A") (.var "B")))

def testRecordDefinition :=
  lspecIO $ group "record definition"
  $ test "no field"
    (parseRecord.run "record A" == .ok (.record dummyPos dummyPos "A" #[]))
  $ test "simple case"
    (parseRecord.run "record N | a : A | b : B" == .ok (.record dummyPos dummyPos "N" #[("a", .var "A"), ("b", .var "B")]))

def main : IO UInt32 :=
  return [
    ← lspecIO $ test "identifier" (term.run "a" == .ok "a"),
    ← testPiType,
    ← testApp,
    ← testPair,
    ← testSigmaType,
    ← testRecordDefinition
  ].foldl (· + ·) 0
