module Violet.Core.Elab
import Control.App
import public Violet.Core.Term
import public Violet.Core.Eval
import public Violet.Core.DataType
import public Violet.Error.Check

public export
record CheckState where
  constructor MkCheckState
  topCtx : Ctx
  topEnv : GlobalEnv
  dataDefs : DataCtx
  mctx : MetaCtx

export
checkState : Ctx -> CheckState
checkState ctx = MkCheckState ctx [] [] emptyMetaCtx

export
initCheckStateWithCarriedState : CheckState -> Ctx -> CheckState
initCheckStateWithCarriedState carriedCheckState ctx =
  { topCtx := { map := carriedCheckState.topCtx.map } ctx
  } carriedCheckState

export
interface Has [Exception CheckError, State CheckState CheckState] e => Elab e where
  getState : App e CheckState
  putState : CheckState -> App e ()

  updateEnv : Name -> Val -> App e ()
  updateCtx : Name -> VTy -> App e ()

  freshMeta : App e Tm

  -- add inductive data type
  addIndType : Name -> CtorSet -> App e ()
  -- find constructor set for data type
  findCtorSet : Name -> App e CtorSet

  report : CheckErrorKind -> App e a
  addPos : Bounds -> App e a -> App e a
export
Has [Exception CheckError, State CheckState CheckState] e => Elab e where
  getState = get CheckState
  putState = put CheckState

  updateEnv x v = do
    state <- getState
    putState $ { topEnv := (x, v) :: state.topEnv } state
  updateCtx x vty = do
    state <- getState
    putState $ { topCtx := extendCtx state.topCtx x vty } state

  freshMeta = do
    state <- getState
    let (mvar, mctx) = newMeta state.mctx
    putState $ { mctx := mctx } state
    pure $ Meta mvar

  addIndType dataName cs = do
    state <- getState
    putState $ { dataDefs := (dataName, cs) :: state.dataDefs } state
  findCtorSet dataName = do
    state <- getState
    Just cs <- pure $ lookup dataName state.dataDefs
      | Nothing => report $ NotADataType dataName
    pure cs

  report err = do
    state <- get CheckState
    let ctx = state.topCtx
    throw $ MkCheckError ctx.filename ctx.source Nothing err
  addPos bounds app = catch app
    (\ce => case ce.bounds of
      Nothing => let err : CheckError = {bounds := Just bounds} ce in throw err
      Just _ => throw ce)

export
runEval : Elab es => Env -> Tm -> App es Val
runEval env a = do
  state <- getState
  Right b <- pure $ eval state.mctx env a
    | Left e => report $ cast e
  pure b

export
runQuote : Elab es => Env -> Val -> App es Tm
runQuote env a = do
  state <- getState
  Right b <- pure $ quote state.mctx env a
    | Left e => report $ cast e
  pure b
