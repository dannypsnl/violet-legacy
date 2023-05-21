import Violet.Core.Eval

namespace Violet.Core

partial def unify [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (l r : Val) : m Unit := do
  let report l r := throw s!"cannot unify `{repr l}` with `{repr r}`"
  match l, r with
  |  .pi x i a b, .pi x' i' a' b' =>
    if i != i' then report l r
    unify a a'
    unify (← b.apply x) (← b'.apply x')
  | .type, .type => return ()
  -- for neutral
  | .rigid h sp, .rigid h' sp' | .flex h sp, .flex h' sp' =>
    if h != h' then report l r
    match sp, sp' with
    | .mk vs, .mk vs' =>
      for (v, v') in vs.zip vs' do
        unify v v'
  -- meta head neutral can unify with something else
  | .flex h sp, t' | t', .flex h sp => sorry
  | _, _ => report l r

end Violet.Core
