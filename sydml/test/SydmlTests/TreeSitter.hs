module SydmlTests.TreeSitter
  ( tests
  )
  where
--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Program
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testProgram "External Tree-sitter tests"
    "tree-sitter" ["test"]
    (Just "./tree-sitter-sydml")
