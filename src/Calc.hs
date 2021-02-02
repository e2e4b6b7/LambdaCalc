{-# LANGUAGE FlexibleContexts #-}

module Calc
  ( alphaEq
  , betaEq
  , nf
  , subst
  , principlePair
  ) where

import           Control.Monad.Except (MonadError (throwError), forM, forM_)
import qualified Data.HashMap.Strict  as Map
import qualified Data.HashSet         as Set
import           Data.Maybe           (fromJust, isJust)

import           Control.Monad.State  (MonadState (get), StateT, evalStateT,
                                       execStateT, gets, modify)
import           Lambda               (Env (..), Expr (..), Map, SubsTy (..),
                                       Symb, Type (..), appEnv, appSubsTy,
                                       extendEnv)

type Set = Set.HashSet

---- Type deduction
freeTVars :: Type -> Set Symb
freeTVars = (`freeTVarsS` Set.empty)
  where
    freeTVarsS :: Type -> Set Symb -> Set Symb
    freeTVarsS (TVar s)    = Set.insert s
    freeTVarsS (t1 :-> t2) = freeTVarsS t1 . freeTVarsS t2

freeTVarsEnv :: Env -> Set Symb
freeTVarsEnv (Env envMap) =
  Map.foldrWithKey
    (\_ val prev -> prev . Set.union (freeTVars val))
    id
    envMap
    Set.empty

unify :: MonadError String m => Type -> Type -> m SubsTy
unify (TVar s1) t2@(TVar s2)
  | s1 == s2 = return mempty
  | otherwise = return $ SubsTy $ Map.singleton s1 t2
unify (TVar s1) t2
  | Set.member s1 (freeTVars t2) =
    throwError $
    "Can't unify (" ++ show (TVar s1) ++ ") with (" ++ show t2 ++ ")!"
  | otherwise = return $ SubsTy $ Map.singleton s1 t2
unify t1 t2@(TVar _) = unify t2 t1
unify (t11 :-> t12) (t21 :-> t22) = do
  u2 <- unify t12 t22
  u1 <- unify (appSubsTy u2 t11) (appSubsTy u2 t21)
  return $ u1 <> u2

principlePair :: MonadError String m => Expr -> m (Env, Type)
principlePair expr = evalStateT (principlePair' expr) names
  where
    names :: [Type]
    names = [TVar (a : replicate num' '\'') | num' <- [0 ..], a <- ['a' .. 'z']]

principlePair' :: MonadError String m => Expr -> StateT [Type] m (Env, Type)
principlePair' expr = do
  typ <- gets head
  modify tail
  envList <-
    forM
      (Set.toList $ freeVars expr)
      (\fvar -> do
         typ <- gets head
         modify tail
         return (fvar, typ))
  eq <- equationsS (Env $ Map.fromList envList) expr typ
  unifier <- solve (eq [])
  return
    ( Env $ Map.fromList $ map (fmap (appSubsTy unifier)) envList
    , appSubsTy unifier typ)
  where
    solve :: MonadError String m => [(Type, Type)] -> m SubsTy
    solve eq =
      execStateT
        (forM_
           eq
           (\(e1, e2) -> do
              subs <- get
              nextSubs <- unify (appSubsTy subs e1) (appSubsTy subs e2)
              modify (nextSubs <>)))
        mempty
    equationsS ::
         MonadError String m
      => Env
      -> Expr
      -> Type
      -> StateT [Type] m ([(Type, Type)] -> [(Type, Type)])
    equationsS env (Var v) t = do
      vt <- appEnv env v
      return ((vt, t) :)
    equationsS env (e1 :@ e2) t = do
      e2t <- gets head
      modify tail
      f <- equationsS env e1 (e2t :-> t)
      s <- equationsS env e2 e2t
      return $ f . s
    equationsS env (Lam v e) t = do
      vt <- gets head
      modify tail
      lamt <- gets head
      modify tail
      f <- equationsS (extendEnv env v vt) e lamt
      return $ f . ((vt :-> lamt, t) :)

---- Equivalence
freeVars :: Expr -> Set Symb
freeVars = addFreeVars Set.empty
  where
    addFreeVars :: Set Symb -> Expr -> Set Symb
    addFreeVars prev (Var s)   = Set.insert s prev
    addFreeVars prev (x :@ xs) = addFreeVars (addFreeVars prev x) xs
    addFreeVars prev (Lam s x) = Set.delete s $ addFreeVars prev x

subst :: Symb -> Expr -> Expr -> Expr
subst var val = subst' var val (freeVars val) Map.empty
  where
    subst' :: Symb -> Expr -> Set Symb -> Map Symb Symb -> Expr -> Expr
    subst' var val _ renamed (Var s)
      | s == var = val
      | otherwise = Var $ Map.lookupDefault s s renamed
    subst' var val used renamed (expr :@ exprs) =
      subst' var val used renamed expr :@ subst' var val used renamed exprs
    subst' var val used renamed (Lam s expr)
      | s == var = Lam s $ rename used renamed expr
      | Set.member s used = Lam newS $ subst' var val newUsed newRenamed expr
      | otherwise = Lam s $ subst' var val used renamed expr
      where
        newS = getNewName used s
        newUsed = Set.insert newS used
        newRenamed = Map.insert s newS renamed
        getNewName :: Set Symb -> Symb -> Symb
        getNewName used s
          | Set.member s used = getNewName used (s ++ "'")
          | otherwise = s
    rename :: Set Symb -> Map Symb Symb -> Expr -> Expr
    rename _ renamed (Var s) = Var $ Map.lookupDefault s s renamed
    rename used renamed (expr :@ exprs) =
      rename used renamed expr :@ rename used renamed exprs
    rename used renamed (Lam s expr)
      | Set.member s used = Lam newS $ rename newUsed newRenamed expr
      | otherwise = Lam s $ rename used renamed expr
      where
        newS = getNewName used s
        newUsed = Set.insert newS used
        newRenamed = Map.insert s newS renamed
        getNewName :: Set Symb -> Symb -> Symb
        getNewName used s
          | Set.member s used = getNewName used (s ++ "'")
          | otherwise = s

infix 1 `alphaEq`

alphaEq :: Expr -> Expr -> Bool
alphaEq = alphaEq' Map.empty Map.empty
  where
    alphaEq' :: Map Symb Symb -> Map Symb Symb -> Expr -> Expr -> Bool
    alphaEq' renamedB renamedA (Var sA) (Var sB)
      | boundA && boundB =
        (renamedB Map.! sB) == sA && sB == (renamedA Map.! sA)
      | not (boundA || boundB) = sB == sA
      | otherwise = False
      where
        boundA = Map.member sA renamedA
        boundB = Map.member sB renamedB
    alphaEq' renamedB renamedA (exprA :@ exprsA) (exprB :@ exprsB) =
      alphaEq' renamedB renamedA exprA exprB &&
      alphaEq' renamedB renamedA exprsA exprsB
    alphaEq' renamedB renamedA (Lam sA exprA) (Lam sB exprB) =
      alphaEq' newRenamedB newRenamedA exprA exprB
      where
        newRenamedB = Map.insert sB sA renamedB
        newRenamedA = Map.insert sA sB renamedA
    alphaEq' _ _ _ _ = False

nf :: Expr -> Expr
nf expr
  | isJust $ reduceOnce expr = nf $ fromJust $ reduceOnce expr
  | otherwise = expr
  where
    reduceOnce :: Expr -> Maybe Expr
    reduceOnce (Lam s expr :@ expr2) = Just $ subst s expr2 expr
    reduceOnce (Lam s expr)
      | isJust reducedExpr = fmap (Lam s) reducedExpr
      | otherwise = Nothing
      where
        reducedExpr = reduceOnce expr
    reduceOnce (expr :@ expr2)
      | isJust $ reduceOnce expr = fmap (:@ expr2) (reduceOnce expr)
      | otherwise = fmap (expr :@) (reduceOnce expr2)
    reduceOnce _ = Nothing

infix 1 `betaEq`

betaEq :: Expr -> Expr -> Bool
betaEq a b = nf a `alphaEq` nf b
