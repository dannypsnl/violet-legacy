import Violet.Core.Context
import Violet.Core.Unification
import Violet.Ast.Surface

namespace Violet.Core
open Lean
open Violet.Ast

def addPos [Monad m] [MonadExcept String m]
  (s e : Position) (f : m α) : m α := do
  tryCatch f
    fun ε => throw s!"{s.line}:{s.column}: {ε}"

def convertIx [Monad m] [MonadExcept String m]
  (count : Nat) (x : String) : TypCtx → m (Core.Tm × VTy)
  | [] => throw s!"no variable named `{x}`"
  | (x', a) :: xs =>
    if x == x' then return (.var (.ix count), a)
    else convertIx (count + 1) x xs

def freshMeta [Monad m] [MonadState MetaCtx m]
  : m Core.Tm := do
  let ctx ← get
  let mvar := ctx.currentMeta
  modify fun ctx =>
    { ctx with
      currentMeta := mvar + 1
      mapping := ctx.mapping.insert mvar .unsolved
    }
  return .meta mvar

mutual

partial
def ElabContext.infer [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (ctx : ElabContext) (tm : Surface.Tm) : m (Core.Tm × VTy) := do
  match tm with
  | .src startPos endPos tm => addPos startPos endPos (infer ctx tm)
  | .var x => convertIx 0 x ctx.typCtx
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
  | .hole => do
    let meta ← freshMeta
    let a ← ctx.env.eval meta
    let t ← freshMeta
    return (t, a)

partial
def ElabContext.check [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (ctx : ElabContext) (tm : Surface.Tm) (ty : VTy) : m Core.Tm := do
  match tm, ← force ty with
  | .src startPos endPos tm, ty => addPos startPos endPos (check ctx tm ty)
  -- TODO: insert implicit lambda for implicit pi
  | .lam x t, .pi _ _ a b =>
    let t ← (ctx.bind x a).check t (← b.apply ctx.lvl.toNat)
    return .lam x t
  | .let x a t u, a' =>
    let a ← ctx.check a .type 
    let va ← ctx.env.eval a
    let t ← ctx.check t va
    let vt ← ctx.env.eval t
    let u ← (ctx.define x vt a').check u a'
    return .let x a t u
  | .hole, _ => freshMeta
  | t, expected => do
    let report := do
      let l ← quote ctx.lvl expected
      throw s!"cannot unify `{l}` with `{t}`"
    let (t, inferred) ← ctx.infer t
    try unify (ctx.lvl) expected inferred
    catch _ => report
    return t

end

end Violet.Core
