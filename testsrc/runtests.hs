module Main where
import Test.QuickCheck
import Test.QuickCheck.Batch
import qualified Test.HUnit as HU
import Test.HUnit.Utils

test1 = TestCase ("x" @=? "x")

alltests = [TestLabel "test1" test1]

main = do runVerboseTests (tl alltests)
          return ()