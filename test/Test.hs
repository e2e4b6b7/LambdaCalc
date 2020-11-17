import           Test.Tasty.HUnit (Assertion)

import           TestCalc         (testCalc)
import           TestLambda       (testLambda)

main :: Assertion
main = do
  testCalc
  testLambda
