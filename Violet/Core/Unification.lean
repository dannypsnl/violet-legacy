import Violet.Core.Eval

namespace Violet.Core
open Violet.Ast.Core

def solve [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (mvar : MetaVar) (sp : Spine) (rhs : Val) : m Unit := do
  -- let pren ← invert gamma sp
  -- let rhs ← rename mvar pren rhs
  -- let solution := [].eval <| lams (reverse <| map snd sp) rhs
  let solution := sorry
  modify <| fun mctx => mctx.set! mvar (.solved solution)
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
  | .flex h sp, t' | t', .flex h sp => solve h sp t'
  | _, _ => report l r

end Violet.Core
