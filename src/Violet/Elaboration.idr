module Violet.Elaboration

import System
import Control.App
import Control.App.Console
import Data.List
import Data.String
import Text.Bounded
import Violet.Core.Conversion
import Violet.Core.Syntax
import public Violet.Core.Term
import public Violet.Core.Eval
import Violet.Core.DataType
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
	Right b <- pure $ quote env a
		| Left e => report $ cast e
	pure b

mutual
	elab : Elab e => Env -> Ctx -> STm -> App e Tm
	elab env ctx tm = do
		(t, _) <- infer env ctx tm
		pure t

	export
	checkModule : Elab e => List Definition -> App e CheckState
	checkModule [] = getState
	checkModule (d :: ds) = go d *> checkModule ds
		where
			handleDataCase : Tm -> DataCase -> App e (Name, List VTy)
			handleDataCase returned_ty (C x tys) = do
				state <- getState
				let env = MkEnv state.topEnv []
				tys <- for tys (elab env state.topCtx)
				tys' <- for tys (runEval env)
				t' <- runEval env (foldr (\t, s => (Pi "_" t s)) returned_ty tys)
				updateCtx x t'
				updateEnv x (VCtor x [])
				pure (x, tys')

			go : Definition -> App e ()
			go (DSrcPos def) = addPos def.bounds (go def.val)
			go (Data dataName cases) = do
				updateCtx dataName VU
				updateEnv dataName (VData dataName)
				addIndType dataName !(for cases $ handleDataCase (Var dataName))
			go (Def x a t) = do
				state <- getState
				let env = MkEnv state.topEnv []
				a <- check env state.topCtx a VU
				a' <- runEval env a

				t <- check (MkEnv state.topEnv [(x, (VVar x))]) (extendCtx state.topCtx x a') t a'
				t' <- runEval env t
				updateEnv x t'
				updateCtx x a'

	export
	infer : Elab e => Env -> Ctx -> STm -> App e (Tm, VTy)
	infer env ctx tm = case tm of
		SrcPos t => addPos t.bounds (infer env ctx t.val)
		SVar x => case lookupCtx ctx x of
			Nothing => report (NoVar x)
			Just a => pure (Var x, a)
		SU => pure (U, VU)
		SApply t u => do
			(t, VPi _ a b) <- infer env ctx t
				| (_, t') => report $ BadApp !(runQuote env t')
			u <- check env ctx u a
			u' <- runEval env u
			Right b' <- pure $ b u'
				| Left e => report $ cast e
			pure (Apply t u, b')
		SLam {} => report InferLam
		SPi x a b => do
			a <- check env ctx a VU
			b <- check (extendEnv env x (VVar x)) (extendCtx ctx x !(runEval env a)) b VU
			pure (Pi x a b, VU)
		SLet x a t u => do
			a <- check env ctx a VU
			a' <- runEval env a
			t <- check env ctx t a'
			t' <- runEval env t
			(u, ty) <- infer (extendEnv env x t') (extendCtx ctx x a') u
			pure (Let x a t u, ty)
		Hole x => do
			a <- runEval env !(freshMeta)
			t <- freshMeta
			pure (t, a)
		SElim targets cases => do
			tTys <- for targets (infer env ctx)
			(cases', rhs_tys) <- foldlM (elabCase (map snd tTys)) ([], []) cases
			let tm = Elim (map fst tTys) cases'
			pure (tm, !(convTys (ElimInfer tm) env rhs_tys))
			where
				elabPattern : Name -> Name -> List Name -> Maybe (List VTy) -> App e (Pat, List (Name, VTy))
				-- when a pattern seems like a ctor, but it is not in constructor set, it's a variable pattern
				-- for example, `m` is var pattern when pattern matching on `Nat`
				elabPattern head x [] Nothing = pure (PVar head, [(head, VData x)])
				elabPattern head _ vars (Just tys) = pure (PCtor head vars, vars `zip` tys)
				elabPattern head x _ Nothing = report $ BadConstructor head x

				checkCase : Env -> Ctx -> List VTy -> SElimCase -> App e (ElimCase, VTy)
				checkCase env ctx (VData x :: ts) ((head ::: vars) :: pats, rhs) = do
					-- TODO: to enable indexed data type, we will need to extend `findCtorSet` in the future
					-- find a constructor set from definition context
					cs <- findCtorSet x
					(newPat, patCtx) <- elabPattern head x vars (lookup head cs)
					let patEnv : LocalEnv = case newPat of
					      PVar _ => [(head, VVar head)]
					      PCtor _ _ => (vars `zip` (map VVar vars))
					let env' = { local := patEnv ++ env.local } env
					let ctx' = extendCtxWithBinds ctx $ patCtx
					(ECase pats rhs, ty) <- checkCase env' ctx' ts (pats, rhs)
					pure (ECase (newPat :: pats) rhs, ty)
				checkCase env ctx ([]) ([], rhs) = do
					(rhs, ty) <- infer env ctx rhs
					pure (ECase [] rhs, ty)
				checkCase env ctx ts _ = report $ BadElimType !(for ts $ runQuote env)

				elabCase : (List VTy) -> (List ElimCase, List VTy) -> SElimCase -> App e (List ElimCase, List VTy)
				elabCase ttys (cases, tys) elim_case = do
					let (_, rhs) = elim_case
					(c, ty) <- checkCase env ctx ttys elim_case
					pure (c :: cases, ty :: tys)

				convTys : CheckErrorKind -> Env -> List VTy -> App e VTy
				convTys err env (t :: t' :: ts) = do
					Right covertable <- pure $ conv env t t'
						| Left err => report $ cast err
					if covertable
						then pure t
						else report $ TypeMismatch !(runQuote env t) !(runQuote env t')
				convTys err env [t] = pure t
				convTys err env [] = report err

	check : Elab e => Env -> Ctx -> STm -> VTy -> App e Tm
	check env ctx t a = go t a
		where
			go : STm -> VTy -> App e Tm
			go (SrcPos t) a = addPos t.bounds (go t.val a)
			go (SLam x t) (VPi x' a b) = do
				let x' = fresh env x'
				case b (VVar x') of
					Right u => Lam x <$> check (extendEnv env x (VVar x)) (extendCtx ctx x a) t u
					Left err => report $ cast err
			go (SLet x a t u) _ = do
				a <- check env ctx a VU
				a' <- runEval env a
				t <- check env ctx t a'
				u <- check (extendEnv env x !(runEval env t)) (extendCtx ctx x a') u a'
				pure (Let x a t u)
			go _ expected = do
				(t', inferred) <- infer env ctx t
				Right convertable <- pure $ conv env inferred expected
					| Left err => report $ cast err
				if convertable
					then pure t'
					else report $ TypeMismatch !(runQuote env expected) !(runQuote env inferred)
