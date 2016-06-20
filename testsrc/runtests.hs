module Main where

import Test.Tasty

import TestNum
import TestMap
import TestTime

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ testMap
  , testNum
  , testTime
  ]
