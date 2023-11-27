import Violet.Core.Eval

namespace Violet.Core
open Lean
open Violet.Ast.Core

structure PartialRenaming where
  domain : Lvl                -- size of Γ
  codomain : Lvl              -- size of Δ
  rename : HashMap Nat Lvl    -- mapping from Δ vars to Γ vars

def PartialRenaming.lift (pren : PartialRenaming) : PartialRenaming :=
  { pren with
    domain := .lvl <| pren.domain.toNat + 1
    codomain := .lvl <| pren.codomain.toNat + 1
    rename := pren.rename.insert pren.codomain.toNat pren.domain
  }

def invert [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (gamma : Lvl) (sp : Spine) : m PartialRenaming := do
  let (domain, rename) ← go sp.reverse.toList
  return {domain, codomain := gamma, rename}
  where
    go : List Val → m (Lvl × HashMap Nat Lvl)
    | [] => return (.lvl 0, default)
    | t :: sp => do
      let (domain, rename) ← go sp
      match ← force t with
      | .rigid _ (.lvl x) _ =>
        if (rename.find? x).isNone then
          return (.lvl <| domain.toNat + 1, rename.insert x domain)
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
      | .recur name .. => throw s!"recursive point `{name}` should be replaced with rigid in this case"
      | .flex mvar' sp =>
        if mvar == mvar' then throw "occurs check"
        else goSp pren (.meta mvar') sp
      | .rigid name (.lvl x) sp =>
        match pren.rename.find? x with
        | .none => throw s!"scope error {pren.rename.toList}"
        | .some x' => goSp pren (.var name (lvl2Ix pren.domain x')) sp
      | .lam x mode t => .lam x mode <$> go pren.lift (← t.apply (vvar x pren.codomain))
      | .pi x mode a b =>
        .pi x mode
          <$> go pren a
          <*> go pren.lift (← b.apply (vvar x pren.codomain))
      | .pair a b => .pair <$> go pren a <*> go pren b
      | .fst p => .fst <$> go pren p
      | .snd p => .snd <$> go pren p
      | .sigma x a b =>
        .sigma x
          <$> go pren a
          <*> go pren.lift (← b.apply (vvar x pren.codomain))
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
  let solution ← (default : Env).eval <| lams pren.domain rhs
  modify <| fun mctx => { mctx with
    mapping := mctx.mapping.insert mvar (.solved solution)
  }

partial def unify [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (lvl : Lvl) (l r : Val) : m Unit := do
  match ← force l, ← force r with
  | .lam x _ t, .lam _ _ t' =>
    unify (.lvl <| lvl.toNat + 1) (← t.apply (vvar x lvl)) (← t'.apply (vvar x lvl))
  | .fst p, .fst p' | .snd p, .snd p' =>
    unify lvl p p'
  | .pair a b, .pair a' b' =>
    unify lvl a a'
    unify lvl b b'
  | .sigma x a b, .sigma _ a' b' =>
    unify lvl a a'
    unify (.lvl <| lvl.toNat + 1) (← b.apply (vvar x lvl)) (← b'.apply (vvar x lvl))
  | .lam x _ t', t | t, .lam x _ t' =>
    unify (.lvl <| lvl.toNat + 1) (← t.apply (vvar x lvl)) (← t'.apply (vvar x lvl))
  | .pi x mode a b, .pi _ mode' a' b' =>
    if mode != mode' then throw "unify error, Π mode mismatched"
    unify lvl a a'
    unify (.lvl <| lvl.toNat + 1) (← b.apply (vvar x lvl)) (← b'.apply (vvar x lvl))
  | .type, .type => return ()
  -- for neutral
  | .rigid n h sp, .rigid n' h' sp' =>
    if h != h' || sp.size != sp'.size then throw s!"unify error, `{n}` and `{n'}`"
    for (t, t') in sp.zip sp' do
      unify lvl t t'
  | .flex h sp, .flex h' sp' =>
    if h != h' || sp.size != sp'.size then throw "unify error, flex"
    for (t, t') in sp.zip sp' do
      unify lvl t t'
  -- meta head neutral can unify with something else
  | .flex h sp, t' | t', .flex h sp => solve lvl h sp t'
  | _, _ => throw "unify error"

end Violet.Core
