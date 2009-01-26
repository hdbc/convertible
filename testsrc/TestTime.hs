{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

module TestMap where
import TestInfrastructure
import Data.Convertible
import Test.QuickCheck
import Test.QuickCheck.Tools
import Test.QuickCheck.Instances
import qualified Data.Map as Map

propListMap :: [(Int, Int)] -> Result
propListMap x = safeConvert x @?= Right (Map.fromList x)

propMapList :: Map.Map Int Int -> Result
propMapList x = safeConvert x @?= Right (Map.toList x)

allt = [q "[(Int, Int)] -> Map" propListMap,
        q "Map -> [(Int, Int)]" propMapList]
