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
