import Violet.Core.Eval

namespace Violet.Core

def solve [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (mvar : MetaVar) (sp : Spine) (rhs : Val) : m Unit := do
  -- let pren ← invert gamma sp
  -- let rhs ← rename mvar pren rhs
  -- let solution := [].eval <| lams (reverse <| map snd sp) rhs
  -- modify <| fun s =>
  --   { s with metaCtx := s.metaCtx.insert mvar (.sovled solution) }
  return ()

partial def unify [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (l r : Val) : m Unit := do
  let report l r := do
    let l ← quote l
    let r ← quote r
    throw s!"cannot unify `{l}` with `{r}`"
  match l, r with
  |  .pi x i a b, .pi x' i' a' b' =>
    if i != i' then report l r
    unify a a'
    unify (← b.apply x) (← b'.apply x')
  | .type, .type => return ()
  -- for neutral
  | .rigid h (.mk sp), .rigid h' (.mk sp')
  | .flex h (.mk sp), .flex h' (.mk sp') =>
    if h != h' then report l r
    for (v, v') in sp.zip sp' do
      unify v v'
  -- meta head neutral can unify with something else
  | .flex h sp, t' | t', .flex h sp => solve h sp t'
  | _, _ => report l r

end Violet.Core
