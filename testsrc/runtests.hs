module Main where

import Test.Hspec

import qualified TestNum
import qualified TestMap
import qualified TestTime

main = hspec $ TestNum.allt >>
       TestTime.allt >>
       TestMap.allt
