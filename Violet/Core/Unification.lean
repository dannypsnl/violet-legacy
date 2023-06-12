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
  (gamma : Lvl) (sp : Spine) : m PartialRenaming := do
  let (dom, ren) ← go sp.reverse.toList
  return {dom, cod := gamma, ren}
  where
    go : List Val → m (Lvl × HashMap Nat Lvl)
    | [] => return (.lvl 0, default)
    | t :: sp => do
      let (dom, ren) ← go sp
      match ← force t with
      | .rigid _ (.lvl x) _ =>
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
        if mvar == mvar' then throw "occurs check"
        else goSp pren (.meta mvar') sp
      | .rigid name (.lvl x) sp =>
        match pren.ren.find? x with
        | .none => throw s!"scope error {pren.ren.toList}"
        | .some x' => goSp pren (.var name (lvl2Ix pren.dom x')) sp
      | .lam x mode t => .lam x mode <$> go pren.lift (← t.apply (.rigid x pren.cod #[]))
      | .pi x mode a b =>
        .pi x mode
          <$> go pren a
          <*> go pren.lift (← b.apply (.rigid x pren.cod #[]))
      | .pair a b => .pair <$> go pren a <*> go pren b
      | .sigma x a b =>
        .sigma x
          <$> go pren a
          <*> go pren.lift (← b.apply (.rigid x pren.cod #[]))
      | .type => return .type

def lams (l : Lvl) (t : Tm) : Tm := Id.run do
  let mut tm := t
  for i in [0:l.toNat] do
    tm :=  .lam (s!"x{i+1}") .explicit tm
  return tm

def solve [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (gamma : Lvl) (mvar : MetaVar) (sp : Spine) (rhs : Val) : m Unit := do
  if sp.size == 0 then
    modify <| fun mctx => { mctx with mapping := mctx.mapping.insert mvar (.solved rhs) }
    return
  let pren ← invert gamma sp
  let rhs ← rename mvar pren rhs
  let solution ← (default : Env).eval <| lams pren.dom rhs
  modify <| fun mctx => { mctx with
    mapping := mctx.mapping.insert mvar (.solved solution)
  }

partial def unify [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (lvl : Lvl) (l r : Val) : m Unit := do
  match ← force l, ← force r with
  | .lam x _ t, .lam _ _ t' =>
    unify (.lvl <| lvl.toNat + 1) (← t.apply (.rigid x lvl #[])) (← t'.apply (.rigid x lvl #[]))
  | .pair a b, .pair a' b' =>
    unify lvl a a'
    unify lvl b b'
  | .sigma x a b, .sigma _ a' b' =>
    unify lvl a a'
    unify (.lvl <| lvl.toNat + 1) (← b.apply (.rigid x lvl #[])) (← b'.apply (.rigid x lvl #[]))
  | .lam x _ t', t | t, .lam x _ t' =>
    unify (.lvl <| lvl.toNat + 1) (← t.apply (.rigid x lvl #[])) (← t'.apply (.rigid x lvl #[]))
  | .pi x mode a b, .pi _ mode' a' b' =>
    if mode != mode' then throw "unify error"
    unify lvl a a'
    unify (.lvl <| lvl.toNat + 1) (← b.apply (.rigid x lvl #[])) (← b'.apply (.rigid x lvl #[]))
  | .type, .type => return ()
  -- for neutral
  | .rigid n h sp, .rigid n' h' sp' =>
    if h != h' || sp.size != sp'.size then throw s!"unify error, {n} {n'}"
    for (t, t') in sp.zip sp' do
      unify lvl t t'
  | .flex h sp, .flex h' sp' =>
    if h != h' || sp.size != sp'.size then throw "unify error"
    for (t, t') in sp.zip sp' do
      unify lvl t t'
  -- meta head neutral can unify with something else
  | .flex h sp, t' | t', .flex h sp => solve lvl h sp t'
  | _, _ => throw "unify error"

end Violet.Core
