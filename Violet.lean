import Violet.Ast.Surface
import Violet.Parser
import Violet.Core.Elaboration

namespace Violet.Core
open Violet.Ast.Surface (Program)

abbrev ElabM := StateT MetaCtx (ExceptT String IO)

def innerCheck (p : Program)
  : StateT ElabContext (StateT MetaCtx (ExceptT String IO)) Unit := do
  for d in p.definitions do
    IO.println s!"checking {repr d}"
    match d with
    | .def name tele ret_ty body =>
      let ctx ← get

      let a := tele.foldr (λ (x, mode, a) b => .pi mode x a b) ret_ty
      let a ← ctx.check a Val.type (m := ElabM)
      let a ← ctx.env.eval a (m := ElabM)

      let t := tele.foldr (λ (x, _, _) body => .lam x body) body
      let t ← ctx.check t a (m := ElabM)
      let t ← ctx.env.eval t (m := ElabM)

      set <| ctx.define name a t
    | .data name constructors =>
      IO.println "skip data"
  return ()

end Violet.Core
namespace Violet.Ast.Surface
open Violet.Core

def Program.check (p : Program) : IO Unit := do
  let result ← (((innerCheck p).run ElabContext.empty).run #[]).run
  match result with
  | Except.ok _ => IO.println "done"
  | Except.error ε => IO.eprintln s!"fail, error:\n{ε}"

end Violet.Ast.Surface
