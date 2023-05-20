import Violet.Ast.Core

namespace Violet.Core
open Violet.Ast.Core

inductive MetaVar
  | mk (idx : Nat)
deriving Repr, Inhabited, BEq
inductive Lvl
  | mk (idx : Nat)
deriving Repr, Inhabited, BEq
instance : Coe Nat MetaVar := ⟨.mk⟩
instance : Coe Nat Lvl := ⟨.mk⟩

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
  | pi : String → VTy → Closure → Val
  | type : Val
deriving Repr, Inhabited, BEq

inductive VTy
  | mk : Val → VTy
deriving Repr, Inhabited, BEq

end

instance : Coe (Array Val) Env := ⟨.mk⟩
instance : Coe (Array Val) Spine := ⟨.mk⟩
instance : Coe Val VTy := ⟨.mk⟩

def Env.extend (e : Env) (v : Val) : Env :=
  match e with
  | .mk vs => vs.push v
def Spine.extend (sp : Spine) (v : Val) : Spine :=
  match sp with
  | .mk vs => vs.push v

end Violet.Core
