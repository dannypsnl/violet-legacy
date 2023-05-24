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
    match d with
    | .def startPos endPos name tele ret_ty body =>
      let ty : Surface.Typ := tele.foldr (λ (x, mode, a) b => .pi mode x a b) ret_ty
      let val := tele.foldr (λ (x, mode, _) body => .lam mode x body) body
      let ty ← reduceCheck (.src startPos endPos ty) .type
      let val ← reduceCheck (.src startPos endPos val) ty
      set <| (← get).define name val ty
    | .data startPos endPos dataName constructors =>
      -- the type name is the value of that type directly
      -- and this is an axiom, which means we cannot check it but just insert it
      --
      -- TODO: for indexed data type, it will not be like current simple `Nat : Type`
      --       so we will need to change this line
      let ctx ← get
      set <| ctx.bind dataName .type
      -- TODO: record constructors into ElabContext, maps dataTypeIndex to constructors
      --let dataTypeIndex : Val := ctx.lvl.toNat
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
open Violet.Parser

partial
def repl : ProgramM Unit := do
  let ctx ← get
  let stdin ← IO.getStdin
  let expression ← stdin.getLine
  match term.run expression with
  | .ok v =>
    let (v, vty) ← ctx.infer v (m := ElabM)
    let v ← ctx.env.eval v (m := ElabM)
    let v ← quote ctx.lvl v (m := ElabM)
    let vty ← quote ctx.lvl vty (m := ElabM)
    IO.println s!"{ctx.showTm v} : {ctx.showTm vty}"
    repl
  | .error ε => IO.eprintln s!"{ε}"

def Program.check (p : Program) (src : System.FilePath) : IO Unit := do
  IO.println s!"checking {src} ..."
  let result ← (((checkDefinitions p).run ElabContext.empty).run default).run
  match result with
  | Except.ok ((_, elabCtx), metaCtx) =>
    IO.println s!"{src} checked successfully"
    let _ ← ((repl.run elabCtx).run metaCtx).run
  | Except.error ε => IO.eprintln s!"{src}:{ε}"

end Violet.Ast.Surface
