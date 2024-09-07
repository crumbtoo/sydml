module Main (main) where
--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit
import SydmlTests.EmptyInput qualified as EmptyInput
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = EmptyInput.tests
