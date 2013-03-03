{-
Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module TestMap where
import Data.Convertible
  
import Test.QuickCheck.Assertions
import Test.QuickCheck.Property
import Test.Hspec (describe)
import Test.Hspec.QuickCheck
  
import qualified Data.Map as Map

propListMap :: [(Int, Int)] -> Result
propListMap x = safeConvert x ?== Right (Map.fromList x)

propMapList :: Map.Map Int Int -> Result
propMapList x = safeConvert x ?== Right (Map.toList x)

allt = describe "Map tests" $ do
  prop "[(Int, Int)] -> Map" propListMap
  prop "Map -> [(Int, Int)]" propMapList
