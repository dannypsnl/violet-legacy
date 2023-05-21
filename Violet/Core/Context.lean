import Violet.Core.Value

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
    env := ctx.env.extend name name
    typCtx := (name, ty) :: ctx.typCtx
  }
def ElabContext.define (ctx : ElabContext) (name : String) (val : Val) (ty : VTy)
  : ElabContext :=
  { ctx with
    env := ctx.env.extend name val
    typCtx := (name, ty) :: ctx.typCtx
  }

end Violet.Core
