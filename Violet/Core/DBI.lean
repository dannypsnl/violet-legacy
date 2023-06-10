import Violet.Ast.Core

namespace Violet.Core
open Violet.Ast.Core

-- 1. lvl `l` is how deep the current context/environment is
-- 2. lvl `x` is how deep the old context/environment is (when we define the value, how deep it is)
-- 3. Then `l - x - 1` will be the index to the value, from current environment
--
-- computed index
-- ---------> target value <----------------- old level
-- <------------------------------ current level
def lvl2Ix (l x : Lvl) : Ix := .ix (l.toNat - x.toNat - 1)

end Violet.Core
