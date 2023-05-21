import Violet.Core.Value

namespace Violet.Core
open Violet.Ast.Core

def Env.lookup [Monad m] [MonadExcept String m] (x : String) : Env → m Val
  | .mk vs => match vs.lookup x with
    | .some v => return v
    | .none => throw s!"no variable `{x}`"

def vMeta [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (v : MetaVar) : m Val := do
  match ← lookupMeta v with
  | .solved t => return t
  | .unsolved => return .flex v (.mk #[])

mutual

partial def Env.eval [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (env : Env) (tm : Tm) : m Val := do
  match tm with
  | .var x => env.lookup x
  | .app t u => (← env.eval t).apply (← env.eval u)
  | .lam x t => return .lam x (Closure.mk x env t)
  | .pi x m a b => return .pi x m (← env.eval a) (Closure.mk x env b)
  | .let x _ t u => (env.extend x (← env.eval t)).eval u
  | .type => return .type
  | .meta m => vMeta m

partial def Closure.apply
  [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  : Closure -> Val -> m Val
  | .mk x env t, u => (env.extend x u).eval t

partial def Val.apply [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (t : Val) (u : Val) : m Val :=
  match t with
  | .lam _ t    => t.apply u
  | .flex  m sp => return .flex m  (sp.extend u)
  | .rigid x sp => return .rigid x (sp.extend u)
  | _           => throw "violet internal bug at value apply"

end

partial def Val.applySpine
  [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (t : Val) : Spine -> m Val
  | .mk sp => sp.foldlM Val.apply t

partial def force [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (t : Val) : m Val := do
  match t with
  | .flex m sp =>
    match ← lookupMeta m with
    | .solved t => force (← t.applySpine sp)
    | _ => return t
  | t => return t

mutual

partial def quoteSpine [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (t : Tm) : Spine → m Tm
  | .mk arr => do
    let mut init := t
    for u in arr do
      let u' ← quote u
      init := .app init u'
    return init

partial def quote [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (t : Val) : m Tm := do
  match ← force t with
  | .flex m sp => quoteSpine (Tm.meta m) sp
  | .rigid x sp => quoteSpine (Tm.var x) sp
  | .lam x t =>
    return .lam x (← quote (← t.apply (.rigid x (.mk #[]))))
  | .pi x m a b =>
    return .pi x m
      (← quote a)
      (← quote (← b.apply (.rigid x (.mk #[]))))
  | .type => return  .type

end

def nf [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (env : Env) (t : Tm) : m Tm := do quote (← env.eval t)

end Violet.Core
