import LSpec
import Violet.Parser

open LSpec
open Violet.Parser Violet.Ast.Surface

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
      (term.run "{a : Type} → Nat" |> Except.isOk)
    $ test "non dependent"
      (term.run "a → b" |> Except.isOk)
    $ test "test non-unicode"
      (term.run "(a : Type) -> Type" |> Except.isOk)
