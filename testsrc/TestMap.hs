{-
Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module TestMap where
import Data.Convertible
import Test.QuickCheck ((===), Property, Testable, quickCheck)
import qualified Data.Map as Map

-- | [(Int, Int)] -> Map
propListMap :: [(Int, Int)] -> Property
propListMap x = safeConvert x === Right (Map.fromList x)

-- | Map -> [(Int, Int)]
propMapList :: Map.Map Int Int -> Property
propMapList x = safeConvert x === Right (Map.toList x)

q :: Testable prop => String -> prop -> IO ()
q testLabel prop = do
  putStrLn testLabel
  quickCheck prop

allt :: [IO ()]
allt = [q "[(Int, Int)] -> Map" propListMap,
        q "Map -> [(Int, Int)]" propMapList]
