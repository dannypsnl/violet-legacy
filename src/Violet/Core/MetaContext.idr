module Violet.Core.MetaContext
import Control.App
import Data.SortedMap
import Violet.Core.Val
import Violet.Core.Common

public export
data MetaEntry = Solved Val | Unsolved

export
record MetaCtx where
	constructor MkMetaCtx
	map : SortedMap MetaVar MetaEntry
	counter : MetaVar

export
emptyMetaCtx : MetaCtx
emptyMetaCtx = MkMetaCtx empty 0

export
newMeta : MetaCtx -> (MetaVar, MetaCtx)
newMeta ctx = do
	let curCount = ctx.counter
	let newCtx = {counter $= S, map := insert curCount Unsolved ctx.map } ctx
	(curCount, newCtx)
