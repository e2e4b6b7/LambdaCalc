import           Calc
import           Test.Tasty.HUnit (Assertion, (@?=))

main :: Assertion
main = do
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

testSubst :: Assertion
testSubst = do
  Lam "x'" (Var "x'" :@ Var "x") @?=
    subst "y" (Var "x") (Lam "x" $ (Var "x") :@ (Var "y"))
  Lam "y" (Var "x" :@ Var "y") @?=
    subst "y" (Var "z") (Lam "y" $ (Var "x") :@ (Var "y"))
  Lam "x'" (Var "x'" :@ Var "x'") @?=
    subst "y" (Var "x") (Lam "x" $ (Var "x") :@ (Var "x"))
