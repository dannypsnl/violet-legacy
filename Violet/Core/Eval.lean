import Violet.Core.Value

namespace Violet.Core
open Violet.Ast.Core

def Env.lookup [Monad m] [MonadExcept String m] (x : Nat) : Env → m Val
  | .mk vs => return vs.get! x

def vMeta [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (v : MetaVar) : m Val := do
  match ← lookupMeta v with
  | .solved t => return t
  | .unsolved => return .flex v (.mk #[])

mutual

partial def Env.eval [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (env : Env) (tm : Tm) : m Val := do
  match tm with
  | .var (Ix.ix x) => env.lookup x
  | .app t u => (← env.eval t).apply (← env.eval u)
  | .lam x m t => return .lam x m (Closure.mk env t)
  | .pi x m a b => return .pi x m (← env.eval a) (Closure.mk env b)
  | .let _ _ t u => (env.extend (← env.eval t)).eval u
  | .type => return .type
  | .meta m => vMeta m

partial def Closure.apply
  [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  : Closure → Val → m Val
  | .mk env t, u => (env.extend u).eval t

partial def Val.apply [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (t : Val) (u : Val) : m Val :=
  match t with
  | .lam _ _ t    => t.apply u
  | .flex  m sp => return .flex m  (sp.extend u)
  | .rigid x sp => return .rigid x (sp.extend u)
  | _           => throw "violet internal bug at value apply"

end

partial def Val.applySpine
  [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (t : Val) : Spine → m Val
  | .mk sp => sp.foldlM Val.apply t

partial def force [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (t : Val) : m Val := do
  match t with
  | .flex m sp =>
    match ← lookupMeta m with
    | .solved t => force (← t.applySpine sp)
    | _ => return t
  | t => return t

def lvl2Ix (l x : Lvl) : Ix := .ix (l.toNat - x.toNat - 1)

mutual

partial def Spine.quote [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (lvl : Lvl) (t : Tm) : Spine → m Tm
  | .mk arr => do
    let mut init := t
    for u in arr do
      let u' ← quote lvl u
      init := .app init u'
    return init

partial def quote [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (lvl : Lvl) (t : Val) : m Tm := do
  match ← force t with
  | .flex m sp => sp.quote lvl (Tm.meta m)
  | .rigid x sp => sp.quote lvl (Tm.var (lvl2Ix lvl x))
  | .lam x m t => return .lam x m (← quote (.lvl <| lvl.toNat + 1) (← t.apply lvl.toNat))
  | .pi x m a b => return .pi x m (← quote lvl a) (← quote lvl (← b.apply lvl.toNat))
  | .type => return  .type

end

def nf [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (env : Env) (t : Tm) : m Tm := do quote (.lvl (env.length)) (← env.eval t)

end Violet.Core
