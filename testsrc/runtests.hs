module Main where
import Test.QuickCheck
import Test.QuickCheck.Batch
import qualified Test.HUnit as HU
import Test.HUnit.Utils

test1 = HU.TestCase ((HU.@=?) "x" "x")

alltests = [HU.TestLabel "test1" test1]

main = do runVerboseTests (HU.TestList alltests)
          return ()