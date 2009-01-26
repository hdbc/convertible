module Main where
import qualified Test.HUnit as HU
import Test.HUnit.Tools

import qualified TestNum
import qualified TestMap

test1 = HU.TestCase ((HU.@=?) "x" "x")

alltests = [HU.TestLabel "test1" test1,
            tl "TestNum" TestNum.allt,
            tl "TestMap" TestMap.allt]

main = do runVerboseTests (HU.TestList alltests)
          return ()