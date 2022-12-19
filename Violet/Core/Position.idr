module Violet.Core.Position

public export
data Position = MkPos Int Int

export
mkPos : (Int, Int) -> Position
mkPos (line, col) = MkPos (line+1) col

export
Show Position where
  show (MkPos line col) = show line ++ ":" ++ show col ++ ":"
