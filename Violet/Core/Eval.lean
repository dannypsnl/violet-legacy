import Violet.Core.Value

namespace Violet.Core
open Violet.Ast.Core

def Env.lookup (env : Env) (x : Nat) : Val :=
  match env with
  | .mk vs => vs.get! x

mutual

partial def Env.eval (env : Env) : Tm → Val
  | .var x => env.lookup x
  | .app t u => (env.eval t).apply (env.eval u)
  | .lam x t => .lam x (.mk env t)
  | .pi x a b => .pi x (env.eval a) (.mk env b)
  -- | .let _ _ t u        => (env.extend eval env t).eval u
  | .type => .type
  -- | Meta m             => vMeta m

partial def Closure.apply : Closure -> Val -> Val
| (.mk env t), u => (env.extend u).eval t

partial def Val.apply (t : Val) (u : Val) : Val :=
  match t with
  | .lam _ t  => t.apply u
  | .flex  m sp => .flex m  (sp.extend u)
  | .rigid x sp => .rigid x (sp.extend u)
  | _           => panic! "impossible"

partial def vAppSp (t : Val) : Spine -> Val
  | .mk sp => sp.foldl Val.apply t

end

end Violet.Core
