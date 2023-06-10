import Lean.Data.Position
import Violet.Ast.Common

namespace Violet.Ast.Surface
open Lean

structure Pattern where
  -- name of constructor
  ctor : String
  vars : Array String
deriving Repr, BEq

inductive Tm : Type
  | src (s e : Position) (tm : Tm)
  | type
  | var (name : String)
  | «let» (name : String) (ty : Tm) (val : Tm) (body : Tm)
  | «match» (target : Tm) (cases : Array (Pattern × Tm))
  | app (mode : Mode) (fn : Tm) (arg : Tm)
  | pi (mode : Mode) (name : String) (ty : Tm) (body : Tm)
  | lam (mode : Mode) (name : String) (body : Tm)
  | sigma (name : String) (ty : Tm) (body : Tm)
  | pair (fst snd : Tm)
  | hole
  deriving Inhabited
instance : Coe String Tm where
  coe s := Tm.var s
abbrev Typ := Tm

abbrev Telescope := Array <| String × Mode × Typ
abbrev Ctor := String × Array Typ

inductive Definition
  | «def» (s e : Position) (name : String) (tele : Telescope) (ret_ty : Typ) (body : Tm)
  | data (s e : Position) (name : String) (constructors : Array Ctor)
  | record (s e : Position) (name : String) (fields : Array <| String × Typ)

structure Program where
  name : String
  definitions : Array Definition

instance : ToString Pattern where
  toString | {ctor, vars} => s!"{ctor} {vars}"

partial def Tm.toString : Tm → String
  | .src _ _ tm => tm.toString
  | .pair fst snd => s!"({fst.toString}, {snd.toString})"
  | .sigma name ty body => s!"({name} : {ty.toString}) × {body.toString}"
  | .lam .implicit p body => "λ" ++ "{" ++ p ++ "}" ++ s!" => {body.toString}"
  | .lam .explicit p body => s!"λ {p} => {body.toString}"
  | .pi .implicit p ty body =>
    "{" ++ p ++ " : " ++ ty.toString ++ "} → " ++ body.toString
  | .pi .explicit p ty body =>
    "(" ++ p ++ " : " ++ ty.toString ++ ") → " ++ body.toString
  | .app .implicit t u => s!"({t.toString} " ++ "{" ++ u.toString ++ "})"
  | .app .explicit t u => s!"({t.toString} {u.toString})"
  | .var x => x
  | .let p ty val body =>
    s!"let {p} : {ty.toString} := {val.toString} in {body.toString}"
  | .match p cs =>
    s!"match {p.toString}"
      ++
       (cs |> Array.map
        (λ (p, b) => s!"\n| {p} => {b.toString}")
        |> Array.toList
        |> List.toString)
  | .type => "Type"
  | .hole => "!!"
instance : ToString Tm where
  toString := Tm.toString

instance : ToString Telescope where
  toString ts := Id.run do
    let mut r := ""
    for (name, mode, ty) in ts do
      let bind := s!"{name} : {ty}"
      match mode with
      | .implicit =>
        r := r ++ " {" ++ bind ++ "}"
      | .explicit =>
        r := r ++ " (" ++ bind ++ ")"
    return r

end Violet.Ast.Surface
