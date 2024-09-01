module SydmlTests.EmptyInput
  ( tests
  )
  where
--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit
--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Empty input"
  [ testCase "1 + 1 = 2" $
      1 + 1 == 2 @?= True
  ]
