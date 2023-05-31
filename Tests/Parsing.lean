import LSpec
import Violet.Parser

open LSpec
open Violet.Parser Violet.Ast.Surface

-- `src` is recording start and end `Position` information, but bad for testing
-- so here convert them to string will remove position's difference
instance : BEq Tm where
  beq x y := x.toString == y.toString

instance [BEq α] : BEq (Except ε α) where
  beq | .ok x, .ok y => x == y
      | _, _ => false

def main := lspecIO $
  test "identifier" (term.run "a" == .ok "a")
  $ group "pi type" $
    test "explicit"
      (term.run "(a : Type) → Type" == .ok (.pi .explicit "a" .type .type))
    $ test "implicit"
      (term.run "{a : Type} → Nat" == .ok (.pi .implicit "a" .type (.var "Nat")))
    $ test "non dependent"
      (term.run "a → b" == .ok (.pi .explicit "_" (.var "a") (.var "b")))
    $ test "test non-unicode"
      (term.run "(a : Type) -> Type" |> Except.isOk)
  $ group "application" $
    test "$ operator"
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
  $ group "pair" $
    test "base case"
      (term.run "(a, b)" == .ok (.pair (.var "a") (.var "b")))
  $ group "sigma type" $
    test "base case"
      (term.run "(x : A) × B" == .ok (.sigma "x" (.var "A") (.var "B")))
    $ test "base case 2"
      (term.run "(x : A) ** B" == .ok (.sigma "x" (.var "A") (.var "B")))
    $ test "product type abbreviation"
      (term.run "A × B" == .ok (.sigma "_" (.var "A") (.var "B")))
