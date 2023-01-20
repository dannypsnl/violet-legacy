module Violet.Core.MetaContext
import Data.SortedMap
import Violet.Core.Val

export
data MetaEntry = Solved Val | Unsolved

record MetaCtx where
	constructor MkMetaCtx
	map : SortedMap MetaVar MetaEntry
	counter : MetaVar

export
emptyMetaCtx : MetaCtx
emptyMetaCtx = MkMetaCtx empty 0

freshMeta : State [MetaCtx MetaCtx] e => App e MetaVar
freshMeta = do
	ctx <- get MetaCtx
	let curCount = ctx.counter
	put $ {counter $= suc, map := insert curCount ctx.map } ctx
	pure curCount
