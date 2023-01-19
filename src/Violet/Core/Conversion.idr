module Violet.Core.Conversion
import Violet.Core.Common
import Violet.Core.Val
import Violet.Error.Eval

export
conv : Env -> Val -> Val -> Either EvalError Bool
conv env t u = go t u
	where
		go : Val -> Val -> Either EvalError Bool
		go VU VU = pure True
		go (VPi x a b) (VPi _ a' b') = do
			let x' = fresh env x
			pure $ !(conv env a a') && !(conv (extendEnv env x' (VVar x')) !(b (VVar x')) !(b' (VVar x')))
		go (VLam x t) (VLam _ t') = do
			let x = fresh env x
			conv (extendEnv env x (VVar x)) !(t env.global (VVar x)) !(t' env.global (VVar x))
		-- checking eta conversion for Lam
		go (VLam x t) u = do
			let x = fresh env x
			conv (extendEnv env x (VVar x)) !(t env.global (VVar x)) (VApp u (VVar x))
		go u (VLam x t) = do
			let x = fresh env x
			conv (extendEnv env x (VVar x)) (VApp u (VVar x)) !(t env.global (VVar x))
		go (VVar x) (VVar x') = pure $ x == x'
		go (VData x) (VData x') = pure $ x == x'
		go (VApp t u) (VApp t' u') = pure $ !(conv env t t') && !(conv env u u')
		go _ _ = pure False
