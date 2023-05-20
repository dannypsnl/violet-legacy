import Violet.Ast.Core

namespace Violet.Core
open Violet.Ast.Core

mutual

inductive Env
  | mk : Array Val → Env
deriving Repr, Inhabited, BEq

inductive Spine
  | mk : Array Val → Spine
deriving Repr, Inhabited, BEq

inductive Closure
  | mk : Env → Tm → Closure
deriving Repr, Inhabited, BEq

inductive Val
  | flex : MetaVar → Spine → Val
  | rigid : Lvl → Spine → Val
  | lam : String → Closure → Val
  | pi : String → Val → Closure → Val
  | type : Val
deriving Repr, Inhabited, BEq

end

@[reducible]
abbrev VTy := Val

inductive MetaEntry
  | solved (v : Val)
  | unsolved
def MetaCtx := Array MetaEntry

def lookupMeta [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (v : MetaVar) : m MetaEntry := do
  let ctx ← get
  match ctx.get? v with
  | .some e => pure e
  | _ => throw "violet internal bug in meta context"

instance : Coe (Array Val) Env := ⟨.mk⟩
instance : Coe (Array Val) Spine := ⟨.mk⟩

def Env.extend (e : Env) (v : Val) : Env :=
  match e with
  | .mk vs => vs.push v
def Env.size (e : Env) : Nat :=
  match e with
  | .mk vs => vs.size

def Spine.extend (sp : Spine) (v : Val) : Spine :=
  match sp with
  | .mk vs => vs.push v

end Violet.Core
