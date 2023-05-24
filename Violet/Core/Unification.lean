import Violet.Core.Eval

namespace Violet.Core
open Lean
open Violet.Ast.Core

structure PartialRenaming where
  dom : Lvl                -- size of Γ
  cod : Lvl                -- size of Δ
  ren : HashMap Nat Lvl    -- mapping from Δ vars to Γ vars

def PartialRenaming.lift (pren : PartialRenaming) : PartialRenaming :=
  { pren with
    dom := .lvl <| pren.dom.toNat + 1
    cod := .lvl <| pren.cod.toNat + 1
    ren := pren.ren.insert pren.cod.toNat pren.dom
  }

def invert [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (gamma : Lvl) : Spine → m PartialRenaming
  | .mk sp => do
    let (dom, ren) ← go sp.reverse.toList
    return {dom, cod := gamma, ren}
  where
    go : List Val → m (Lvl × HashMap Nat Lvl)
    | [] => return (.lvl 0, {})
    | t :: sp => do
      let (dom, ren) ← go sp
      match ← force t with
      | .rigid (.lvl x) _ =>
        if (ren.find? x).isNone then
          return (.lvl <| dom.toNat + 1, ren.insert x dom)
        else throw "cannot unify"
      | _ => throw "cannot unify"

partial
def rename [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (mvar : MetaVar) (pren : PartialRenaming) (val : Val)
  : m Tm := go pren val
  where
    goSp (pren : PartialRenaming) (t : Tm) : Spine → m Tm
      | .mk sp => sp.foldrM (fun u t => do return Tm.app t (← go pren u)) t

    go (pren : PartialRenaming) (t : Val) : m Tm := do
      match ← force t with
      | .flex mvar' sp =>
        if mvar == mvar' then
          throw "occurs check"
        else
          goSp pren (.meta mvar') sp
      | .rigid (.lvl x) sp =>
        match pren.ren.find? x with
        | .none => throw "scope error"
        | .some x' => goSp pren (.var (lvl2Ix pren.dom x')) sp
      | .lam x t => .lam x <$> go pren.lift (← t.apply pren.cod.toNat)
      | .pi x mode a b => .pi x mode <$> go pren a <*> go pren.lift (← b.apply pren.cod.toNat)
      | .type => return .type
      
def lams (l : Lvl) (t : Tm) : Tm := Id.run do
  let mut tm := t
  for i in [0:l.toNat] do
    tm :=  .lam (s!"x{i+1}") tm
  return tm

def solve [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (gamma : Lvl) (mvar : MetaVar) (sp : Spine) (rhs : Val) : m Unit := do
  let pren ← invert gamma sp
  let rhs ← rename mvar pren rhs
  let solution ← (Env.mk []).eval <| lams pren.dom rhs
  modify <| fun mctx => { mctx with mapping := mctx.mapping.insert mvar (.solved solution)}
  return ()

partial def unify [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (lvl : Lvl) (l r : Val) : m Unit := do
  let report l r := do
    let l ← quote lvl l
    let r ← quote lvl r
    throw s!"cannot unify `{l}` with `{r}`"
  match l, r with
  |  .pi _ i a b, .pi _ i' a' b' =>
    if i != i' then report l r
    unify lvl a a'
    unify lvl (← b.apply lvl.toNat) (← b'.apply lvl.toNat)
  | .type, .type => return ()
  -- for neutral
  | .rigid h (.mk sp), .rigid h' (.mk sp')
  | .flex h (.mk sp), .flex h' (.mk sp') =>
    if h != h' then report l r
    for (v, v') in sp.zip sp' do
      unify lvl v v'
  -- meta head neutral can unify with something else
  | .flex h sp, t' | t', .flex h sp => solve lvl h sp t'
  | _, _ => report l r

end Violet.Core
