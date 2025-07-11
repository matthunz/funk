{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Funk.Solver where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.IORef
import Funk.Infer (Constraint (..))
import Funk.STerm
import qualified Funk.Subst as S
import Funk.Term
import Funk.Token
import System.IO.Unsafe
import Text.Parsec

data SError
  = InfiniteType (Either SourcePos (Located Ident))
  | UnificationError SType SType
  | MissingTraitImpl SourcePos STBinding SType

newtype Solver a = Solver {unSolver :: ExceptT [SError] (StateT S.Env IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState S.Env, MonadError [SError])

runSolver :: Solver a -> S.Env -> IO (Either [SError] a)
runSolver solver env = fst <$> runStateT (runExceptT $ unSolver solver) env

prune :: SType -> Solver SType
prune ty@(TVar ref) = do
  b <- liftIO $ readIORef ref
  case b of
    Bound ty' -> do
      ty'' <- prune ty'
      liftIO $ writeIORef ref (Bound ty'')
      return ty''
    _ -> return ty
prune (TArrow t1 t2) = TArrow <$> prune t1 <*> prune t2
prune (TForall v t) = TForall v <$> prune t
prune (TApp t1 t2) = TApp <$> prune t1 <*> prune t2

freshUnboundTyS :: SourcePos -> Solver STBinding
freshUnboundTyS pos = do
  env <- get
  let idx = S.envNextIdx env
  put env {S.envNextIdx = idx + 1}
  liftIO $ newIORef (Unbound pos idx)

unify :: SType -> SType -> Solver ()
unify t1 t2 = do
  ta <- prune t1
  tb <- prune t2
  pos <- liftIO $ typePos ta
  case (ta, tb) of
    (TVar v1, TVar v2) | v1 == v2 -> return ()
    (TVar v1, TVar v2) -> do
      v1' <- liftIO $ readIORef v1
      v2' <- liftIO $ readIORef v2
      case (v1', v2') of
        (Skolem _ id1, Skolem _ id2) ->
          when (id1 /= id2) $ throwError [UnificationError ta tb]
        (Unbound _ id1, Unbound _ id2) ->
          if id1 < id2
            then bindVar v2 ta
            else bindVar v1 tb
        (Unbound {}, Skolem {}) -> bindVar v1 tb
        (Skolem {}, Unbound {}) -> bindVar v2 ta
        _ -> throwError [UnificationError ta tb]
    (TVar v, r) -> do
      v' <- liftIO $ readIORef v
      case v' of
        Skolem {} -> throwError [UnificationError (TVar v) r]
        Unbound {} -> bindVar v r
        _ -> throwError [UnificationError (TVar v) r]
    (l, TVar v) -> do
      v' <- liftIO $ readIORef v
      case v' of
        Skolem {} -> throwError [UnificationError l (TVar v)]
        Unbound {} -> bindVar v l
        _ -> throwError [UnificationError l (TVar v)]
    (TArrow a1 a2, TArrow b1 b2) -> unify a1 b1 >> unify a2 b2
    (TForall v1 t1', TForall v2 t2') -> do
      fresh <- freshUnboundTyS pos
      let t1Subst = substituteTypeVar v1 (TVar fresh) t1'
      let t2Subst = substituteTypeVar v2 (TVar fresh) t2'
      unify t1Subst t2Subst
    (TForall v t, other) -> do
      fresh <- freshUnboundTyS pos
      let tSubst = substituteTypeVar v (TVar fresh) t
      unify tSubst other
    (other, TForall v t) -> do
      fresh <- freshUnboundTyS pos
      let tSubst = substituteTypeVar v (TVar fresh) t
      unify other tSubst
    _ -> throwError [UnificationError ta tb]

substituteTypeVar :: STBinding -> SType -> SType -> SType
substituteTypeVar old new ty = case ty of
  TVar ref | ref == old -> new
  TVar ref -> TVar ref
  TArrow t1 t2 -> TArrow (substituteTypeVar old new t1) (substituteTypeVar old new t2)
  TForall v t | v == old -> TForall v t
  TForall v t -> TForall v (substituteTypeVar old new t)
  TApp t1 t2 -> TApp (substituteTypeVar old new t1) (substituteTypeVar old new t2)

bindVar :: STBinding -> SType -> Solver ()
bindVar v ty = do
  occurs <- occursCheck v ty
  when occurs $ do
    v' <- liftIO $ readIORef v
    case v' of
      Skolem i _ -> throwError [InfiniteType $ Right i]
      Unbound pos _ -> throwError [InfiniteType $ Left pos]
      _ -> return ()
  liftIO $ writeIORef v (Bound ty)

occursCheck :: STBinding -> SType -> Solver Bool
occursCheck v t = do
  t' <- prune t
  case t' of
    TVar v' -> return (v == v')
    TArrow x y -> (||) <$> occursCheck v x <*> occursCheck v y
    TForall v' th -> if v == v' then return False else occursCheck v th
    TApp t1 t2 -> (||) <$> occursCheck v t1 <*> occursCheck v t2

solve :: [Constraint] -> Solver ()
solve = mapM_ go
  where
    go (CEq t1 t2) = unify t1 t2
    go (CTrait traitName typeArgs targetType) = solveTrait traitName typeArgs targetType

solveTrait :: STBinding -> [STBinding] -> SType -> Solver ()
solveTrait traitName typeArgs targetType = do
  env <- get
  -- Prune the target type to get its canonical form
  prunedTarget <- prune targetType

  -- Look for matching implementation in envImpls
  case findMatchingImpl traitName typeArgs prunedTarget (S.envImpls env) of
    Just _impl -> return () -- Implementation found, constraint satisfied
    Nothing -> do
      pos <- liftIO $ typePos targetType
      throwError [MissingTraitImpl pos traitName targetType]

-- Check if two traits match by comparing their names
traitsMatch :: STBinding -> STBinding -> IO Bool
traitsMatch trait1 trait2 = do
  name1 <- sTBindingToIdent trait1
  name2 <- sTBindingToIdent trait2
  return (name1 == name2)

-- Find a matching trait implementation
findMatchingImpl :: STBinding -> [STBinding] -> SType -> [(STBinding, [STBinding], SType, SStmt)] -> Maybe SStmt
findMatchingImpl traitName _typeArgs targetType impls =
  case [ impl | (implTrait, _implVars, implType, impl) <- impls, let traitMatch = unsafePerformIO $ traitsMatch traitName implTrait, let typeMatch = typesMatch targetType implType, traitMatch && typeMatch
       ] of
    (impl : _) -> Just impl
    [] -> Nothing

-- Check if two types match (simplified unification check)
typesMatch :: SType -> SType -> Bool
typesMatch (TVar ref1) (TVar ref2) = ref1 == ref2
typesMatch (TApp t1 t2) (TApp t1' t2') = typesMatch t1 t1' && typesMatch t2 t2'
typesMatch (TArrow t1 t2) (TArrow t1' t2') = typesMatch t1 t1' && typesMatch t2 t2'
typesMatch _ _ = False

solveConstraints :: [Constraint] -> S.Env -> IO (Either [SError] ())
solveConstraints cs env = do
  res' <- runSolver (solve cs) env
  case res' of
    Left errs -> return (Left errs)
    Right () -> return (Right ())
