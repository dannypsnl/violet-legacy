import Violet.Core.Eval
import Violet.Ast.Surface

namespace Violet.Core
open Violet.Ast

@[reducible]
abbrev TypCtx := List (String × VTy)

structure ElabContext where
  env : Env
  lvl : Lvl
  typCtx : TypCtx
  mctx : MetaCtx

def ElabContext.bind (ctx : ElabContext) (name : String) (ty : VTy)
  : ElabContext :=
  { ctx with
    env := ctx.env.extend name <| Val.rigid name (.mk #[])
    typCtx := (name, ty) :: ctx.typCtx
  }
def ElabContext.define (ctx : ElabContext) (name : String) (val : Val) (ty : VTy)
  : ElabContext :=
  { ctx with
    env := ctx.env.extend name val
    typCtx := (name, ty) :: ctx.typCtx
  }

mutual

partial
def infer [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
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
  | .lam x t => throw "cannot infer lambda without type annotation"
  -- infer `t u`
  -- TODO: create syntax to handle implicit application
  | .app t u =>
    let (t, ty) ← infer ctx t
    match ← force ty with
    | .pi x _ a b =>
      let u ← check ctx u a
      return (.app t u, ← b.apply <| ← ctx.env.eval u)
    | _ => throw "cannot apply non-function"
  | .type => return (.type, .type)
  -- infer `(x : a) -> b`
  | .pi mode x a b =>
    let a' ← check ctx a .type
    let b' ← check (ctx.bind x (← ctx.env.eval a')) b .type
    return (.pi x mode a' b', .type)
  | .let x a t u =>
    let a ← check ctx a .type
    let va ← ctx.env.eval a
    let t ← check ctx t va
    let vt ← ctx.env.eval t
    let (u, b) ← infer (ctx.define x vt va) u
    return (.let x a t u, b)
  -- FIXME: This is obivously wrong, just for filling the case
  | .match target cases =>
    let mut rTy := Option.none
    for (pat, body) in cases do
      let (_, bTy) ← infer ctx body
      rTy := bTy
    let (target, _) ← infer ctx target
    match rTy with
    | .some r => return (target, r)
    | .none => throw ""

partial
def check [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (ctx : ElabContext) (tm : Surface.Tm) (ty : VTy) : m Core.Tm := do
  sorry

end

end Violet.Core
