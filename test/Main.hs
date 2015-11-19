module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.Basic
-------------------------------------------------------------------------------
import           Test.Tasty.Ingredients.FailFast
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMainWithIngredients [ listingTests
                                  , failFast consoleTestReporter] tests


-------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "tasty-fail-fast"
  [
    testCase "trivial pass" (True @?= True)
  , testCase "first failure" (True @?= False)
  , testCase "another pass" (True @?= True)
  , testCase "second failure" (True @?= False)
  , testCase "yet another pass" (True @?= True)
  ]
