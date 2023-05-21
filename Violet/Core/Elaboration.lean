import Violet.Core.Context
import Violet.Core.Unification
import Violet.Ast.Surface

namespace Violet.Core
open Violet.Ast

mutual

partial
def ElabContext.infer [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (ctx : ElabContext) (tm : Surface.Tm) : m (Core.Tm × VTy) := do
  match tm with
  | .var x =>
    match ctx.typCtx.lookup x with
    | .some a => return (.var x, a)
    | .none => throw s!"no variable named `{x}`"
  -- TODO: a good idea would be having two lambda forms
  -- 1. lam x => t
  -- 2. lam (x : T) => t
  --
  -- The first one cannot be inferred, but the second one can.
  | .lam .. => throw "cannot infer lambda without type annotation"
  -- infer `t u`
  -- TODO: create syntax to handle implicit application
  | .app t u =>
    let (t, ty) ← ctx.infer t
    match ← force ty with
    | .pi _x _ a b =>
      let u ← ctx.check u a
      return (.app t u, ← b.apply <| ← ctx.env.eval u)
    | _ => throw "cannot apply non-function"
  | .type => return (.type, .type)
  -- infer `(x : a) -> b`
  | .pi mode x a b =>
    let a' ← ctx.check a .type
    let b' ← (ctx.bind x (← ctx.env.eval a')).check b .type
    return (.pi x mode a' b', .type)
  | .let x a t u =>
    let a ← ctx.check a .type
    let va ← ctx.env.eval a
    let t ← ctx.check t va
    let vt ← ctx.env.eval t
    let (u, b) ← infer (ctx.define x vt va) u
    return (.let x a t u, b)
  -- FIXME: This is obivously wrong, just for filling the case
  | .match target cases =>
    let mut rTy := Option.none
    for (_pat, body) in cases do
      let (_, bTy) ← ctx.infer body
      rTy := bTy
    let (target, _) ← ctx.infer target
    match rTy with
    | .some r => return (target, r)
    | .none => throw ""

partial
def ElabContext.check [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (ctx : ElabContext) (tm : Surface.Tm) (ty : VTy) : m Core.Tm := do
  match tm, ← force ty with
  | .lam x t, .pi _ _ a b =>
    let t ← (ctx.bind x a).check t (← b.apply x)
    return .lam x t
  | .let x a t u, a' =>
    let a ← ctx.check a .type 
    let va ← ctx.env.eval a
    let t ← ctx.check t va
    let vt ← ctx.env.eval t
    let u ← (ctx.define x vt a').check u a'
    return .let x a t u
  | t, expected => do
    let (t, inferred) ← ctx.infer t
    unify expected inferred
    return t

end

end Violet.Core
