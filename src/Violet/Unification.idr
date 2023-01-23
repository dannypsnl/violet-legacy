module Violet.Unification
import Control.App
import Data.SortedMap
import Violet.Core.Syntax
import Violet.Core.Elab

ty_mismatch : Elab e => Env -> VTy -> VTy -> App e ()
ty_mismatch env t u = report $ TypeMismatch !(runQuote env t) !(runQuote env u)

mutual
	export
	unify : Elab e => Env -> VTy -> VTy -> App e ()
	unify env t u = go t u
	where
		go : Val -> Val -> App e ()
		-- unification
		-- go (VMeta m) (VMeta m') = if m == m' then pure () else ty_mismatch env t u
		go (VMeta m) u = solve env m u
		go t (VMeta m) = solve env m t
		-- conversion
		go VU VU = pure ()
		go (VPi _ x a b) (VPi _ _ a' b') = do
			let x' = VVar $ fresh env x
			Right l <- pure $ b x'
				| Left e => report $ cast e
			Right r <- pure $ b' x'
				| Left e => report $ cast e
			unify env a a'
			unify (extendEnv env x x') l r
		go (VLam x t) (VLam _ t') = do
			let x = fresh env x
			Right l <- pure $ t env.global (VVar x)
				| Left e => report $ cast e
			Right r <- pure $ t' env.global (VVar x)
				| Left e => report $ cast e
			unify (extendEnv env x (VVar x)) l r
		-- checking eta conversion for Lam
		go (VLam x t) u = do
			let x = fresh env x
			Right l <- pure $ t env.global (VVar x)
				| Left e => report $ cast e
			unify (extendEnv env x (VVar x)) l (VApp u (VVar x))
		go u (VLam x t) = do
			let x = fresh env x
			Right l <- pure $ t env.global (VVar x)
				| Left e => report $ cast e
			unify (extendEnv env x (VVar x)) (VApp u (VVar x)) l
		go (VVar x) (VVar x') = if x == x' then pure () else ty_mismatch env t u
		go (VData x) (VData x') = if x == x' then pure () else ty_mismatch env t u
		go (VApp t u) (VApp t' u') = do
			unify env t t'
			unify env u u'
		go _ _ = ty_mismatch env t u

	solve : Elab e => Env -> MetaVar -> Val -> App e ()
	solve env mvar val = do
		state <- getState
		case lookup mvar (state.mctx.map) of
			Just (Solved v) => unify env val v
			Just Unsolved => do
				let mctx' : MetaCtx = { map := insert mvar (Solved val) state.mctx.map } state.mctx
				putState $ { mctx := mctx' } state
			-- nothing case is basically impossible, unless compiler has bug
			Nothing => ty_mismatch env (VMeta mvar) val
