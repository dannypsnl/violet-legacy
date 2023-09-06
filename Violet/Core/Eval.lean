import Violet.Core.Value
import Violet.Core.DBI

namespace Violet.Core
open Violet.Ast.Core

def Env.lookup [Monad m] [MonadExcept String m] (vs : Env) : Ix → m Val
  | .ix x => return vs.get! x

def vMeta [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (v : MetaVar) : m Val := do
  match ← lookupMeta v with
  | .solved t => return t
  | .unsolved => return .flex v #[]

def Env.matching? [Monad m] [MonadExcept String m]
  (env : Env) (pat : Pattern) : Val → m (Env × Bool)
  | .rigid _ h sp => do
    if h.toNat != pat.ctor.toNat || sp.size != pat.vars.size then
      return (env, false)
    else
      return (sp.foldl (fun env v => env.extend v) env, true)
  | _ => throw "cannot match on non-rigid value"

mutual

partial def Env.eval [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (env : Env) (tm : Tm) : m Val := do
  match tm with
  | .var _ x => env.lookup x
  | .app t u => (← env.eval t).apply (← env.eval u)
  | .pair fst snd => return .pair (← env.eval fst) (← env.eval snd)
  | .fst p => (← env.eval p).proj₁
  | .snd p => (← env.eval p).proj₂
  | .sigma x a b => return .sigma x (← env.eval a) (Closure.mk env b)
  | .lam x m t => return .lam x m (Closure.mk env t)
  | .pi x m a b => return .pi x m (← env.eval a) (Closure.mk env b)
  | .let _ _ t u => (env.extend (← env.eval t)).eval u
  | .type => return .type
  | .meta m => vMeta m
  | .match target cases =>
    let t ← env.eval target
    for (pat, clause) in cases do
      let (env', matched) ← env.matching? pat t
      if matched then
        return ← env'.eval clause
      else continue
    throw "no clause matched"

partial def Val.proj₁ [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (self : Val) : m Val :=
  match self with
  | .pair fst _ => return fst
  | p => return .fst p
partial def Val.proj₂ [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (self : Val) : m Val :=
  match self with
  | .pair _ snd => return snd
  | p => return .snd p

partial def Closure.apply
  [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  : Closure → Val → m Val
  | .mk (env : Env) t, u => (env.extend u).eval t

partial def Val.apply [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (t : Val) (u : Val) : m Val :=
  match t with
  | .lam x m (.mk (.recur _ lvl :: (env : Env)) t) =>
    ((env.extend (.lam x m (.mk (.recur x lvl :: env) t))).extend u).eval t
  | .lam _ _ t => t.apply u
  | .flex  m (sp : Spine) => return .flex m (sp.extend u)
  | .rigid n x (sp : Spine) => return .rigid n x (sp.extend u)
  -- recur will only get applied in check phase
  | .recur n lvl => return .rigid n lvl #[u]
  | _ => throw "violet internal bug at value apply"

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
  | .recur name x => return .var name (lvl2Ix lvl x)
  | .flex m (sp : Spine) => sp.quote lvl (.meta m)
  | .rigid name x (sp : Spine) => sp.quote lvl (.var name (lvl2Ix lvl x))
  | .pair fst snd => return .pair (← quote lvl fst) (← quote lvl snd)
  | .fst p => return .fst (← quote lvl p)
  | .snd p => return .fst (← quote lvl p)
  | .sigma x a b => return .sigma x (← quote lvl a) (← quote (.lvl <| lvl.toNat + 1) (← b.apply (vvar x lvl)))
  | .lam x m t => return .lam x m (← quote (.lvl <| lvl.toNat + 1) (← t.apply (vvar x lvl)))
  | .pi x m a b => return .pi x m (← quote lvl a) (← quote (.lvl <| lvl.toNat + 1) (← b.apply (vvar x lvl)))
  | .type => return  .type

end

def nf [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (env : Env) (t : Tm) : m Tm := do quote (.lvl 0) (← env.eval t)

end Violet.Core
