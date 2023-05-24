import Violet.Ast.Surface
import Violet.Parser
import Violet.Core.Elaboration

namespace Violet.Core
open Violet.Ast
open Violet.Ast.Surface (Program)

abbrev ElabM := StateT MetaCtx (ExceptT String IO)
abbrev ProgramM := StateT ElabContext (StateT MetaCtx (ExceptT String IO))

def reduceCheck (tm : Surface.Tm) (vty : VTy) : ProgramM Val := do
  let ctx ← get
  let tm ← ctx.check tm vty (m := ElabM)
  return ← ctx.env.eval tm (m := ElabM)

def checkDefinitions (p : Program) : ProgramM Unit := do
  for d in p.definitions do
    IO.println s!"checking\n{d}"
    match d with
    | .def startPos endPos name tele ret_ty body =>
      let ty : Surface.Typ := tele.foldr (λ (x, mode, a) b => .pi mode x a b) ret_ty
      let val := tele.foldr (λ (x, _, _) body => .lam x body) body
      let ctx ← get
      let ty ← reduceCheck (.src startPos endPos ty) .type
      let val ← reduceCheck (.src startPos endPos val) ty
      set <| ctx.define name val ty
    | .data startPos endPos dataName constructors =>
      -- the type name is the value of that type directly
      -- and this is an axiom, which means we cannot check it but just insert it
      set <| (← get).bind dataName .type
      for (name, tys) in constructors do
        let ty := tys.foldr (λ ty b => .pi .explicit "_" ty b) (.var dataName)
        let ty ← reduceCheck (.src startPos endPos ty) .type
        -- A constructor is just a rigid binding in the environment
        --
        -- e.g. `true` will have value `.rigid true`
        --
        -- so we use `coe` here for constructor name too
        set <| (← get).bind name ty

end Violet.Core

namespace Violet.Ast.Surface
open Violet.Core

def Program.check (p : Program) : IO Unit := do
  let result ← (((checkDefinitions p).run ElabContext.empty).run {}).run
  match result with
  | Except.ok _ => IO.println "done"
  | Except.error ε => IO.eprintln s!"fail, error:\n{ε}"

end Violet.Ast.Surface
