module TestLambda where

import           Test.Tasty.HUnit (Assertion, assertBool)

import           Calc             (betaEq)
import           Lambda           (Expr (..))

testLambda :: Assertion
testLambda = do
  testReadShow

testReadShow :: Assertion
testReadShow = do
  testReadShowOne (Lam "x" $ Lam "y" $ Var "x")
  testReadShowOne (Lam "z" (Var "r" :@ Var "z"))
  testReadShowOne (Lam "x" $ Lam "y" $ (Lam "z" $ Var "z") :@ Var "x")
  testReadShowOne ((Var "x" :@ Var "y") :@ Var "z")
  testReadShowOne (Var "x" :@ (Var "y" :@ Var "z"))
  testReadShowOne (Var "x\'" :@ Var "_y" :@ Var "Z")
  testReadShowOne (Lam "x" (Var "y"))
  where
    testReadShowOne :: Expr -> Assertion
    testReadShowOne e = assertBool (show e) (e `betaEq` (read $ show e :: Expr))
