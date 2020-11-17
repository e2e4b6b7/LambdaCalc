module Calc
  ( alphaEq
  , betaEq
  , nf
  , subst
  ) where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet        as Set
import           Data.Maybe          (fromJust, isJust)

import           Lambda              (Expr (..), Symb)

type Map = Map.HashMap

type Set = Set.HashSet

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
