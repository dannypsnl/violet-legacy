import Violet.Core.Eval
import Violet.Ast.Surface

namespace Violet.Core
open Violet.Ast

@[reducible]
abbrev TypCtx := List (String × VTy)

structure ElabContext where
  env : Env
  typCtx : TypCtx
  mctx : MetaCtx

def ElabContext.empty : ElabContext := {
    env := .mk []
    typCtx := []
    mctx := #[]
  }

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
  | .lam .. => throw "cannot infer lambda without type annotation"
  -- infer `t u`
  -- TODO: create syntax to handle implicit application
  | .app t u =>
    let (t, ty) ← infer ctx t
    match ← force ty with
    | .pi _x _ a b =>
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
    for (_pat, body) in cases do
      let (_, bTy) ← infer ctx body
      rTy := bTy
    let (target, _) ← infer ctx target
    match rTy with
    | .some r => return (target, r)
    | .none => throw ""

partial
def check [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (ctx : ElabContext) (tm : Surface.Tm) (ty : VTy) : m Core.Tm := do
  match tm, ty with
  | t, expected => do
    let (t, inferred) ← infer ctx t
    unify expected inferred
    return t

end

end Violet.Core
