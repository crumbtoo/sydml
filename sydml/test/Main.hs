module Main (main) where
--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit
import SydmlTests.EmptyInput qualified as EmptyInput
import SydmlTests.TreeSitter qualified as TreeSitter
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "sydml"
  [ EmptyInput.tests
  -- , TreeSitter.tests
  ]
