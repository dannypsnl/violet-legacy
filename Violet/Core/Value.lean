import Violet.Ast.Core

namespace Violet.Core
open Violet.Ast.Core
open Violet.Ast

inductive Lvl
  | lvl (v : Nat)
  deriving Repr, Inhabited, BEq
def Lvl.toNat : Lvl → Nat
  | .lvl v => v

mutual

inductive Env
  | mk (mapping : List Val)
  deriving Repr, Inhabited, BEq

inductive Spine
  | mk : Array Val → Spine
  deriving Repr, Inhabited, BEq

inductive Closure
  | mk (name : String) : Env → Tm → Closure
  deriving Repr, Inhabited, BEq

/-- Val

`flex` and `rigid` are both stands for neutral terms,

1. but `flex` has a meta head
2. and `rigid` has a bound variable head

Let's say we have usual `Nat` definition, then `suc n` is `rigid`, but `a n` is flex where `{a : Nat → Nat}`
-/
inductive Val
  | flex (head : MetaVar) (body : Spine)
  | rigid (head : Lvl) (body : Spine)
  | lam (name : String) (clos : Closure)
  | pi (name : String) (mode : Surface.Mode) (ty : Val) (clos : Closure)
  | type
  deriving Repr, Inhabited, BEq

end

instance : Coe Nat Val where
  coe x := Val.rigid (.lvl x) (.mk #[])

@[reducible]
abbrev VTy := Val

inductive MetaEntry
  | solved (v : Val)
  | unsolved
@[reducible]
def MetaCtx := Array MetaEntry

def lookupMeta [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (v : MetaVar) : m MetaEntry := do
  let ctx ← get
  match ctx.get? v with
  | .some e => pure e
  | _ => throw "violet internal bug in meta context"

def Env.extend (v : Val) : Env → Env
  | .mk vs => .mk <| v :: vs
def Env.length : Env → Nat
  | .mk vs => vs.length

instance : Coe (Array Val) Spine := ⟨.mk⟩

def Spine.extend (v : Val) : Spine → Spine
  | .mk vs => vs.push v

end Violet.Core
