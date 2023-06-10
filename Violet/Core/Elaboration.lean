import Violet.Core.Context
import Violet.Core.Unification
import Violet.Ast.Surface

namespace Violet.Core
open Lean
open Violet.Ast
open Violet.Ast.Core

def addPos [Monad m] [MonadExcept String m]
  (s e : Position) (f : m α) : m α := do
  tryCatch f
    fun ε => throw s!"{s.line}:{s.column}: {ε}"

def nameToIndex [Monad m] [MonadExcept String m]
  (count : Nat) (x : String) : TypCtx → m (Core.Tm × VTy)
  | [] => throw s!"no variable named `{x}`"
  | (x', a) :: xs =>
    if x == x' then return (.var (.ix count), a)
    else nameToIndex (count + 1) x xs

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

def nameToLevel [Monad m] [MonadExcept String m]
  (x : String) : TypCtx → m (Lvl × VTy)
  | [] => throw s!"no variable named `{x}`"
  | (x', a) :: xs =>
    if x == x' then return (.lvl xs.length, a)
    else nameToLevel x xs

partial def mkPatternCtx [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (patTy : Core.Tm) (patVars : List String) (ctx : ElabContext) : m ElabContext := do
  match patTy, patVars with
  | .pi _ _ ty body, name :: rest =>
    let ctx' := ctx.bind name (← ctx.env.eval ty)
    mkPatternCtx (← quote ctx'.lvl (← ctx.env.eval body))
      rest
      ctx'
  | .var _, _ => return ctx
  | _, _ => throw "bad pattern type"

mutual

partial
def ElabContext.infer [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (ctx : ElabContext) (tm : Surface.Tm) : m (Core.Tm × VTy) := do
  match tm with
  | .src startPos endPos tm => addPos startPos endPos (infer ctx tm)
  | .var x => nameToIndex 0 x ctx.typCtx
  -- infer application like
  -- 1. `t u` explicit
  -- 2. `t {u}` implicit
  | .app appMode t u =>
    let (t', ty) ← ctx.infer t
    match ← force ty with
    | .pi _ piMode a b =>
      if appMode == piMode then
        let u ← ctx.check u a
        return (.app t' u, ← b.apply <| ← ctx.env.eval u)
      -- In this case, appMode is explicit, we insert a hole for this application
      else if piMode == .implicit then
        ctx.infer (.app appMode (.app .implicit t .hole) u)
      else
        throw s!"bad mode {t} {u}"
    | ty =>
      let t ← quote ctx.lvl ty
      throw s!"non appliable type `{ctx.showTm t}`"
  | .type => return (.type, .type)
  -- infer `(x : a) -> b`
  | .pi mode x a b =>
    let a' ← ctx.check a .type
    let b' ← (ctx.bind x (← ctx.env.eval a')).check b .type
    return (.pi x mode a' b', .type)
  -- infer `(x : a) × b`
  | .sigma x a b =>
    let a' ← ctx.check a .type 
    let b' ← (ctx.bind x (← ctx.env.eval a')).check b .type
    return (.sigma x a' b', .type)
  | .let x a t u =>
    let a ← ctx.check a .type
    let va ← ctx.env.eval a
    let t ← ctx.check t va
    let vt ← ctx.env.eval t
    let (u, b) ← infer (ctx.define x vt va) u
    return (.let x a t u, b)
  | .hole => do
    let meta ← freshMeta
    let a ← ctx.env.eval meta
    let t ← freshMeta
    return (t, a)
  -- TODO: a good idea would be having two lambda forms
  -- 1. lam x => t
  -- 2. lam (x : T) => t
  --
  -- The first one cannot be inferred, but the second one can.
  | .lam .. => throw "cannot infer lambda without type annotation"
  | .pair .. => throw "cannot infer pair"
  | .match .. => throw "cannot infer pattern matching"

partial
def ElabContext.check [Monad m] [MonadState MetaCtx m] [MonadExcept String m]
  (ctx : ElabContext) (tm : Surface.Tm) (ty : VTy) : m Core.Tm := do
  match tm, ← force ty with
  | .src startPos endPos tm, ty => addPos startPos endPos (check ctx tm ty)
  -- if lambda has same mode as pi type, then just check it
  | .lam m@.implicit x t, .pi _ .implicit a b 
  | .lam m@.explicit x t, .pi _ .explicit a b  =>
    let t ← (ctx.bind x a).check t (← b.apply ctx.lvl.toNat)
    return .lam x m t
  -- otherwise if pi type is implicit, insert a new implicit lambda
  | t, .pi x .implicit a b =>
    let t ← (ctx.bind x a).check t (← b.apply ctx.lvl.toNat)
    return .lam x .implicit t
  -- pair has a sigma type
  | .pair fst snd, .sigma _ a b =>
    let fst' ← ctx.check fst a
    let snd' ← ctx.check snd (← b.apply <| ← ctx.env.eval fst')
    return .pair fst' snd'
  | .let x a t u, a' =>
    let a ← ctx.check a .type 
    let va ← ctx.env.eval a
    let t ← ctx.check t va
    let vt ← ctx.env.eval t
    let u ← (ctx.define x vt a').check u a'
    return .let x a t u
  | .hole, _ => freshMeta
  -- match at here should be desugared pattern matching
  | .match target cases, expected =>
    let (target, dataType) ← ctx.infer target
    if let (Val.rigid dataTypeLvl _) := dataType then
      let ctors := ctx.dataTypeCtx.find! dataTypeLvl
      let mut coreCases := #[]
      for (pat, body) in cases do
        -- 1. pat has ctor name
        -- 2. pat has variables
        -- A context we should create here is a mapping from these variables to types
        -- then check body with this context has the expected type
        let (patLvl, patTy) ← nameToLevel pat.ctor ctx.typCtx
        if !ctors.contains patLvl then
          let dataType ← quote ctx.lvl dataType
          let constructor ← quote ctx.lvl (.rigid patLvl #[])
          throw s!"data type `{ctx.showTm dataType}` has no constructor named `{ctx.showTm constructor}`"
        let patTy ← quote ctx.lvl patTy
        -- intro pattern context
        let ctx' ← mkPatternCtx patTy pat.vars.toList ctx
        let body ← ctx'.check body expected
        coreCases := coreCases.push
          ({ctor := patLvl, vars := pat.vars}, body)
      return .match target coreCases
    else throw "match target must has a rigid type"
  | t, expected => do
    let (t', inferred) ← ctx.infer t
    try unify ctx.lvl expected inferred
    catch msg =>
      let e ← quote ctx.lvl expected
      let i ← quote ctx.lvl inferred
      throw s!"checking {t}\n  cannot unify `{ctx.showTm e}` with `{ctx.showTm i}`\n  {msg}"
    return t'

end

end Violet.Core
