import Violet.Core.Value
import Lean.Data.HashSet

namespace Violet.Core
open Violet.Ast.Core

-- 1. lvl `l` is how deep the current context/environment is
-- 2. lvl `x` is how deep the old context/environment is (when we define the value, how deep it is)
-- 3. Then `l - x - 1` will be the index to the value, from current environment
--
-- computed index
-- ---------> target value <----------------- old level
-- <------------------------------ current level
def lvl2Ix (l x : Lvl) : Ix := .ix (l.toNat - x.toNat - 1)

@[reducible]
abbrev TypCtx := List (String × VTy)

structure ElabContext where
  env : Env
  typCtx : TypCtx
  mctx : MetaCtx
  dataTypeCtx : Lean.HashMap Lvl (Lean.HashSet Lvl)
  lvl : Lvl

def ElabContext.empty : ElabContext := {
    env := .mk []
    typCtx := []
    mctx := default
    dataTypeCtx := default
    lvl := .lvl 0
  }

def ElabContext.bind (ctx : ElabContext) (name : String) (ty : VTy)
  : ElabContext :=
  let (.lvl curLvl) := ctx.lvl
  { ctx with
    lvl := .lvl <| curLvl + 1
    env := ctx.env.extend curLvl
    typCtx := (name, ty) :: ctx.typCtx
  }
def ElabContext.define (ctx : ElabContext) (name : String) (val : Val) (ty : VTy)
  : ElabContext :=
  let (.lvl curLvl) := ctx.lvl
  { ctx with
    lvl := .lvl <| curLvl + 1
    env := ctx.env.extend val
    typCtx := (name, ty) :: ctx.typCtx
  }
def ElabContext.addConstructor (ctx : ElabContext) (dataType ctor : Lvl) : ElabContext :=
  let ctors := ctx.dataTypeCtx.findD dataType Lean.HashSet.empty
  { ctx with
    dataTypeCtx := ctx.dataTypeCtx.insert dataType (ctors.insert ctor)
  }

-- misc: pretty print
partial def ElabContext.showPat (ctx : ElabContext) (pat : Pattern) : String :=
  let (name, _) := ctx.typCtx.get! <| lvl2Ix ctx.lvl pat.ctor
  name ++ " " ++ toString pat.vars

partial def ElabContext.showTm (ctx : ElabContext) : Tm → String
  | .lam p .implicit body => "λ " ++ "{" ++ p ++ "}" ++ s!" => {ctx.showTm body}"
  | .lam p .explicit body => s!"λ {p} => {ctx.showTm body}"
  | .pi p .implicit ty body =>
    "{" ++ p ++ " : " ++ ctx.showTm ty ++ "} → " ++ ctx.showTm body
  | .pi p .explicit ty body =>
    "(" ++ p ++ " : " ++ ctx.showTm ty ++ ") → " ++ ctx.showTm body
  | .app t u => s!"{ctx.showTm t} {ctx.showTm u}"
  | .var (.ix x) => let (name, _) := ctx.typCtx.get! x; name
  | .meta m => s!"?{m}"
  | .let p ty val body =>
    s!"let {p} : {ctx.showTm ty} := {ctx.showTm val} in {ctx.showTm body}"
  | .match t cases => Id.run do
    let mut s := s!"match {ctx.showTm t}"
    for (p, body) in cases do
      s := s ++ s!"| {ctx.showPat p} => {ctx.showTm body}"
    return s
  | .type => "Type"

end Violet.Core
