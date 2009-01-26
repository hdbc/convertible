module Main where
import qualified Test.HUnit as HU
import Test.HUnit.Tools

import qualified TestNum

test1 = HU.TestCase ((HU.@=?) "x" "x")

alltests = [HU.TestLabel "test1" test1,
            tl "TestNum" TestNum.allt]

main = do runVerboseTests (HU.TestList alltests)
          return ()