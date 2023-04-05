module Violet.Elaboration
import Control.App
import Data.SortedMap
import Violet.Core.Syntax
import public Violet.Core.Elab
import Violet.Unification

mutual
  export
  checkModule : Elab e => List Definition -> App e CheckState
  checkModule [] = getState
  checkModule (d :: ds) = go d *> checkModule ds
    where
      handleDataCase : Tm -> DataCase -> App e (Name, List VTy)
      handleDataCase returned_ty (C x tys) = do
        state <- getState
        let env = MkEnv state.topEnv []
        tys <- for tys (\stm => do (v, _) <- infer env state.topCtx stm; pure v)
        tys' <- for tys (runEval env)
        t' <- runEval env (foldr (\t, s => (Pi Explicit "_" t s)) returned_ty tys)
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
      (t, VPi mode x a b) <- infer env ctx t
        | (_, bad_ty) => report $ BadApp !(runQuote env bad_ty)
      case mode of
        Explicit => do
          -- check u : a
          u <- check env ctx u a
          u' <- runEval env u
          Right b' <- pure $ b u'
            | Left e => report $ cast e
          pure (Apply t u, b')
        Implicit => do
          -- insert a hole
          -- and check it has type `a`
          m <- check env ctx (Hole (fresh env x)) a
          m' <- runEval env m
          Right (VPi mode x a b') <- pure $ b m'
            | Right bad_ty => report $ BadApp !(runQuote env bad_ty)
            | Left e => report $ cast e
          u <- check env ctx u a
          u' <- runEval env u
          Right b'' <- pure $ b' u'
            | Left e => report $ cast e
          pure (Apply (Apply t m) u, b'')
    SLam {} => report InferLam
    SPi mode x a b => do
      a <- check env ctx a VU
      b <- check (extendEnv env x (VVar x)) (extendCtx ctx x !(runEval env a)) b VU
      pure (Pi mode x a b, VU)
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
      (ty :: tys) <- pure rhs_tys
        | [] => report $ ElimInfer tm
      ty <- foldlM (\a, b => do
        unify env a b
        -- report $ TypeMismatch !(runQuote env a) !(runQuote env b)
        pure b) ty tys
      pure (tm, ty)
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

  check : Elab e => Env -> Ctx -> STm -> VTy -> App e Tm
  check env ctx t a = go t a
    where
      go : STm -> VTy -> App e Tm
      go (SrcPos t) a = addPos t.bounds (go t.val a)
      go (SLam x t) (VPi _ _ a b) = do
        let x' = VVar $ fresh env x
        Right u <- pure $ b x'
          | Left err => report $ cast err
        Lam x <$> check (extendEnv env x x') (extendCtx ctx x a) t u
      go (SLet x a t u) _ = do
        a <- check env ctx a VU
        a' <- runEval env a
        t <- check env ctx t a'
        u <- check (extendEnv env x !(runEval env t)) (extendCtx ctx x a') u a'
        pure (Let x a t u)
      go _ expected = do
        (t', inferred) <- infer env ctx t
        unify env expected inferred
        pure t'
