module SydmlTests.EmptyInput
  ( tests
  )
  where
--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import qualified Data.ByteString.Lazy as BS
--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Empty input"
  [
    -- goldenVsString
    -- "Parse"
    -- "sydml/golden/Empty module/output/Parse"
    -- (BS.readFile "sydml/golden/Empty module/Empty.sydml")
  ]
