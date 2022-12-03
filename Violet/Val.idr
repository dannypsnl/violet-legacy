module Violet.Val

import Violet.Syntax

mutual
  public export
  data Val
    = VVar Name
    | VApp Val Val
    | VLam Name (Val -> Val)
    | VPi Name VTy (Val -> Val)
    | VU

  public export
  VTy : Type
  VTy = Val
