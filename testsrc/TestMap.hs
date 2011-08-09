{-
Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module TestMap where
import TestInfrastructure
import Data.Convertible
import Test.QuickCheck
import Test.QuickCheck.Tools
import Test.QuickCheck.Instances
import qualified Test.QuickCheck.Property as P
import qualified Data.Map as Map

propListMap :: [(Int, Int)] -> P.Result
propListMap x = safeConvert x @?= Right (Map.fromList x)

propMapList :: Map.Map Int Int -> P.Result
propMapList x = safeConvert x @?= Right (Map.toList x)

allt = [q "[(Int, Int)] -> Map" propListMap,
        q "Map -> [(Int, Int)]" propMapList]
