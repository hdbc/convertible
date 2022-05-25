module Main where

import qualified TestNum
import qualified TestMap
import qualified TestTime

main :: IO ()
main = do
  sequence_ TestNum.allt
  sequence_ TestMap.allt
  sequence_ TestTime.allt
