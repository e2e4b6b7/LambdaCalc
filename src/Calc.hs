module Calc where

import           Data.HashMap.Strict as Map (HashMap, empty, insert,
                                             lookupDefault, member)
import           Data.HashSet        as Set (HashSet, delete, empty, insert,
                                             member)

type Symb = String

infixl 2 :@

data Expr
  = Var Symb
  | Expr :@ Expr
  | Lam Symb Expr
  deriving (Eq, Read, Show)

freeVars :: Expr -> HashSet Symb
freeVars = addFreeVars Set.empty
  where
    addFreeVars :: HashSet Symb -> Expr -> HashSet Symb
    addFreeVars prev (Var s)   = Set.insert s prev
    addFreeVars prev (x :@ xs) = addFreeVars (addFreeVars prev x) xs
    addFreeVars prev (Lam s x) = Set.delete s $ addFreeVars prev x

rename :: HashMap Symb Symb -> Expr -> Expr
rename renamed (Var s) = Var $ lookupDefault s s renamed
rename renamed (expr :@ exprs) = rename renamed expr :@ rename renamed exprs
rename renamed (Lam s expr)
  | Map.member s renamed = undefined
  | otherwise = Lam s $ rename renamed expr

subst :: Symb -> Expr -> Expr -> Expr
subst var val = subst' var val (freeVars val) Map.empty
  where
    subst' :: Symb -> Expr -> HashSet Symb -> HashMap Symb Symb -> Expr -> Expr
    subst' var val _ renamed (Var s)
      | s == var = val
      | otherwise = Var $ lookupDefault s s renamed
    subst' var val used renamed (expr :@ exprs) =
      subst' var val used renamed expr :@ subst' var val used renamed exprs
    subst' var val used renamed (Lam s expr)
      | s == var = Lam s $ rename renamed expr
      | Set.member s used = Lam newS $ subst' var val newUsed newRenamed expr
      | otherwise = Lam s $ subst' var val used renamed expr
      where
        newS = getNewName used s
        newUsed = Set.insert newS used
        newRenamed = Map.insert s newS renamed
        getNewName :: HashSet Symb -> Symb -> Symb
        getNewName used s
          | Set.member s used = getNewName used (s ++ "'")
          | otherwise = s

infix 1 `alphaEq`

alphaEq :: Expr -> Expr -> Bool
alphaEq = alphaEq' Map.empty
  where
    alphaEq' :: HashMap Symb Symb -> Expr -> Expr -> Bool
    alphaEq' renamed (Var sA) (Var sB) = sB == lookupDefault sA sA renamed
    alphaEq' renamed (exprA :@ exprsA) (exprB :@ exprsB) =
      alphaEq' renamed exprA exprB && alphaEq' renamed exprsA exprsB
    alphaEq' renamed (Lam sA exprA) (Lam sB exprB)
      | sA == sB = alphaEq' renamed exprA exprB
      | otherwise = alphaEq' newRenamed exprA exprB
      where
        newRenamed = Map.insert sA sB renamed
