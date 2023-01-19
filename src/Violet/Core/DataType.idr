module Violet.Core.DataType
import Violet.Core.Common
import Violet.Core.Val

public export
CtorSet : Type
CtorSet = List (Name, List VTy)

public export
DataCtx : Type
DataCtx = List (Name, CtorSet)
