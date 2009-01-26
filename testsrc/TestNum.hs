{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

module TestNum where
import TestInfrastructure
import Data.Convertible
import Test.QuickCheck
import Test.QuickCheck.Tools
import Data.Word

prop_int_to_integer :: Int -> Result
prop_int_to_integer x =
    safeConvert x @?= Right ((fromIntegral x)::Integer)

prop_integer_to_int_pass :: Integer -> Property
prop_integer_to_int_pass x =
    (x <= fromIntegral (maxBound :: Int)) &&
    (x >= fromIntegral (minBound :: Int)) ==> 
                                          safeConvert x @?= Right ((fromIntegral x)::Int)

prop_integer_to_word8 :: Integer -> Result
prop_integer_to_word8 x =
    safeConvert x @?= if x >= fromIntegral (minBound :: Word8) &&
                         x <= fromIntegral (maxBound :: Word8)
                      then Right ((fromIntegral x)::Word8)
                      else Left $ ConvertError (show x) "Integer" "Word8" "Input value outside of bounds: (0,255)"

prop_integer_to_word8_safe :: Integer -> Property
prop_integer_to_word8_safe x =
    x <= fromIntegral (maxBound :: Word8) &&
    x >= fromIntegral (minBound :: Word8) ==>
      safeConvert x @?= Right ((fromIntegral x)::Word8)

prop_integer_to_word8_unsafe :: Integer -> Property
prop_integer_to_word8_unsafe x =
    x < fromIntegral (minBound :: Word8) ||
    x > fromIntegral (maxBound :: Word8) ==>
      ((safeConvert x)::ConvertResult Word8) @?= (Left $ ConvertError (show x) "Integer" "Word8" "Input value outside of bounds: (0,255)")

prop_double_to_word8 :: Double -> Result
prop_double_to_word8 x =
    safeConvert x @?= if truncate x >= toInteger (minBound :: Word8) &&
                         truncate x <= toInteger (maxBound :: Word8)
                      then Right ((truncate x)::Word8)
                      else Left $ ConvertError (show x) "Double" "Word8" "Input value outside of bounds: (0,255)"

prop_double_to_word8_safe :: Double -> Property
prop_double_to_word8_safe x =
    x <= fromIntegral (maxBound :: Word8) &&
    x >= fromIntegral (minBound :: Word8) ==>
      safeConvert x @?= Right ((truncate x)::Word8)

prop_double_to_word8_unsafe :: Double -> Property
prop_double_to_word8_unsafe x =
    truncate x < toInteger (minBound :: Word8) ||
    truncate x > toInteger (maxBound :: Word8) ==>
      ((safeConvert x)::ConvertResult Word8) @?= (Left $ ConvertError (show x) "Double" "Word8" "Input value outside of bounds: (0,255)")

allt = [q "Int -> Integer" prop_int_to_integer,
        q "Integer -> Int (safe bounds)" prop_integer_to_int_pass,
        q "Integer -> Word8 (general)" prop_integer_to_word8,
        q "Integer -> Word8 (safe bounds)" prop_integer_to_word8_safe,
        q "Integer -> Word8 (unsafe bounds)" prop_integer_to_word8_unsafe,
        q "Double -> Word8 (general)" prop_double_to_word8,
        q "Double -> Word8 (safe bounds)" prop_double_to_word8_safe,
        q "Double -> Word8 (unsafe bounds)" prop_double_to_word8_unsafe
       ]


