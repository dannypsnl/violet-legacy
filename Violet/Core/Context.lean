import Violet.Core.Value

namespace Violet.Core
open Violet.Ast

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
    mctx := #[]
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

end Violet.Core
