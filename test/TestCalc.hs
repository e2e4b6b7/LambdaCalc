module TestCalc where

import           Test.Tasty.HUnit (Assertion, (@?=))

import           Calc             (alphaEq, subst)
import           Lambda           (Expr (..))

testCalc :: Assertion
testCalc = do
  testSubst
  testAlphaEq

testAlphaEq :: Assertion
testAlphaEq = do
  True @?=
    ((Lam "x" $ Lam "y" $ Var "x") `alphaEq` (Lam "y" $ Lam "x" $ Var "y"))
  False @?=
    ((Lam "x" $ Lam "y" $ Var "z") `alphaEq` (Lam "y" $ Lam "x" $ Var "y"))
  False @?=
    ((Lam "x" $ Lam "y" $ Var "x") `alphaEq` (Lam "y" $ Lam "x" $ Var "x"))
  True @?= ((Var "x") `alphaEq` (Var "x"))
  False @?= ((Var "x") `alphaEq` (Var "y"))
  True @?=
    ((Lam "x" (Var "y" :@ Var "x")) `alphaEq` (Lam "z" (Var "y" :@ Var "z")))
  False @?=
    ((Lam "y" (Var "y" :@ Var "x")) `alphaEq` (Lam "z" (Var "y" :@ Var "z")))
  False @?=
    ((Lam "x" (Var "y" :@ Var "x")) `alphaEq` (Lam "z" (Var "r" :@ Var "z")))
  False @?= ((Lam "x" (Var "y")) `alphaEq` (Lam "y" (Var "y")))
  False @?= ((Lam "y" (Var "y")) `alphaEq` (Lam "x" (Var "y")))
  True @?= ((Lam "x" (Var "y")) `alphaEq` (Lam "z" (Var "y")))
  True @?= ((Var "x" :@ Var "y") `alphaEq` (Var "x" :@ Var "y"))
  False @?= ((Var "x" :@ Var "y") `alphaEq` (Var "x" :@ Var "x"))
  False @?=
    ((Var "x" :@ (Lam "y" $ Var "x")) `alphaEq` (Var "y" :@ (Lam "x" $ Var "x")))
  False @?=
    ((Var "x" :@ (Lam "y" $ Var "x")) `alphaEq` (Var "x" :@ (Lam "x" $ Var "x")))
  True @?=
    ((Var "x" :@ (Lam "y" $ Var "x")) `alphaEq` (Var "x" :@ (Lam "z" $ Var "x")))
  True @?=
    ((Var "x" :@ (Lam "x" $ Var "x")) `alphaEq` (Var "x" :@ (Lam "x" $ Var "x")))
  False @?=
    ((Var "x" :@ (Lam "y" $ Var "x")) `alphaEq` (Var "x" :@ (Lam "x" $ Var "x")))
  False @?=
    ((Lam "x" $ Lam "y" $ Var "x") `alphaEq` (Lam "x" $ Lam "x" $ Var "x"))

testSubst :: Assertion
testSubst = do
  Lam "x'" (Var "x'" :@ Var "x") @?=
    subst "y" (Var "x") (Lam "x" $ (Var "x") :@ (Var "y"))
  Lam "y" (Var "x" :@ Var "y") @?=
    subst "y" (Var "z") (Lam "y" $ (Var "x") :@ (Var "y"))
  Lam "x'" (Var "x'" :@ Var "x'") @?=
    subst "y" (Var "x") (Lam "x" $ (Var "x") :@ (Var "x"))
  (Lam "x'" $ Lam "y" $ (Var "x'") :@ (Var "x'")) @?=
    subst "y" (Var "x") (Lam "x" $ Lam "y" $ (Var "x") :@ (Var "x"))
  (Lam "x'" $ Lam "y" $ Lam "x''" $ (Var "x'") :@ (Var "x''")) @?=
    subst "y" (Var "x") (Lam "x" $ Lam "y" $ Lam "x'" $ (Var "x") :@ (Var "x'"))
  (Lam "x'" $ Lam "x''" $ (Var "x'") :@ (Var "x''")) @?=
    subst "y" (Var "x") (Lam "x" $ Lam "x'" $ (Var "x") :@ (Var "x'"))
