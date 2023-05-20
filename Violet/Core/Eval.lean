import Violet.Core.Value

namespace Violet.Core
open Violet.Ast.Core

def Env.lookup (env : Env) (x : Nat) : Val :=
  match env with
  | .mk vs => vs.get! x

def vMeta [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (v : MetaVar) : m Val := do
  match ← lookupMeta v with
  | .solved t => return t
  | .unsolved => return .flex v (.mk #[])

mutual

partial def Env.eval [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (env : Env) : Tm → m Val
  | .var x => return env.lookup x
  | .app t u => do (← env.eval t).apply (← env.eval u)
  | .lam x t => return .lam x (.mk env t)
  | .pi x a b => return .pi x (← env.eval a) (.mk env b)
  -- | .let _ _ t u => (env.extend eval env t).eval u
  | .type => return .type
  | .meta m => vMeta m

partial def Closure.apply
  [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  : Closure -> Val -> m Val
| (.mk env t), u => (env.extend u).eval t

partial def Val.apply [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (t : Val) (u : Val) : m Val :=
  match t with
  | .lam _ t    => t.apply u
  | .flex  m sp => return .flex m  (sp.extend u)
  | .rigid x sp => return .rigid x (sp.extend u)
  | _           => throw "violet internal bug at value apply"

partial def Val.applySpine
  [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (t : Val) : Spine -> m Val
  | .mk sp => sp.foldlM Val.apply t

end

partial def force [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (t : Val) : m Val := do
  match t with
  | .flex m sp =>
    match ← lookupMeta m with
    | .solved t => force (← t.applySpine sp)
    | _ => return t
  | t => return t

def lvl2Ix : Lvl → Lvl → Nat
  | l, x => (l - x - 1)

mutual

partial def quoteSpine [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (l : Lvl) (t : Tm) (sp : Spine) : m Tm := do
  match sp with
  | .mk arr =>
    let mut init := t
    for u in arr do
      let u' ← quote l u
      init := .app init u'
    return init

partial def quote [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (l : Nat) (t : Val) : m Tm := do
  match ← force t with
  | .flex m sp => quoteSpine l (Tm.meta m) sp
  | .rigid x sp => quoteSpine l (Tm.var (lvl2Ix l x)) sp
  | .lam x t =>
    return .lam x (← quote (l + 1) (← t.apply (.rigid l (.mk #[]))))
  | .pi x a b =>
    return .pi x
      (← quote l a)
      (← quote (l + 1) (← b.apply (.rigid l (.mk #[]))))
  | .type => return  .type

end

def nf [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (env : Env) (t : Tm) : m Tm := do
  quote env.size (← env.eval t)

end Violet.Core
