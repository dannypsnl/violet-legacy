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
    | [] => return (.lvl 0, default)
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
      | .lam x mode t => .lam x mode <$> go pren.lift (← t.apply pren.cod.toNat)
      | .pi x mode a b =>
        .pi x mode
          <$> go pren a
          <*> go pren.lift (← b.apply pren.cod.toNat)
      | .type => return .type

def lams (l : Lvl) (t : Tm) : Tm := Id.run do
  let mut tm := t
  for i in [0:l.toNat] do
    tm :=  .lam (s!"x{i+1}") .explicit tm
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
  match l, r with
  | .lam _ _ t, .lam _ _ t' =>
    unify (.lvl <| lvl.toNat + 1) (← t.apply lvl.toNat) (← t'.apply lvl.toNat)
  | .lam _ _ t', t | t, .lam _ _ t' =>
    unify (.lvl <| lvl.toNat + 1) (← t.apply lvl.toNat) (← t'.apply lvl.toNat)
  | .pi _ mode a b, .pi _ mode' a' b' =>
    if mode != mode' then throw "unify error"
    unify lvl a a'
    unify (.lvl <| lvl.toNat + 1) (← b.apply lvl.toNat) (← b'.apply lvl.toNat)
  | .type, .type => return ()
  -- for neutral
  | .rigid h (.mk sp), .rigid h' (.mk sp')
  | .flex h (.mk sp), .flex h' (.mk sp') =>
    if h != h' || sp.size != sp'.size then throw "unify error"
    for (t, t') in sp.zip sp' do
      unify lvl t t'
  -- meta head neutral can unify with something else
  | .flex h sp, t' | t', .flex h sp => solve lvl h sp t'
  | _, _ => throw "unify error"

end Violet.Core
