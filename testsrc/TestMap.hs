{-
Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module TestMap(testMap) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Convertible
import qualified Data.Map as Map

propListMap :: [(Int, Int)] -> Bool
propListMap x = safeConvert x == Right (Map.fromList x)

propMapList :: Map.Map Int Int -> Bool
propMapList x = safeConvert x == Right (Map.toList x)

testMap = testGroup "TestMap" [
    testProperty "[(Int, Int)] -> Map" propListMap
  , testProperty "Map -> [(Int, Int)]" propMapList
  ]
