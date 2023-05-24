import Violet.Core.Value

namespace Violet.Core
open Violet.Ast.Core

@[reducible]
abbrev TypCtx := List (String × VTy)

structure ElabContext where
  env : Env
  typCtx : TypCtx
  mctx : MetaCtx
  lvl : Lvl

def ElabContext.empty : ElabContext := {
    env := .mk []
    typCtx := []
    mctx := default
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

def ElabContext.showTm (ctx : ElabContext) : Tm → String
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
  | .type => "Type"

end Violet.Core
