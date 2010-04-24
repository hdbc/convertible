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
import qualified Test.QuickCheck.Property as P
import Data.Word

prop_int_to_integer :: Int -> P.Result
prop_int_to_integer x =
    safeConvert x @?= Right ((fromIntegral x)::Integer)

prop_integer_to_int_pass :: Integer -> Property
prop_integer_to_int_pass x =
    (x <= fromIntegral (maxBound :: Int)) &&
    (x >= fromIntegral (minBound :: Int)) ==> 
                                          safeConvert x @?= Right ((fromIntegral x)::Int)

prop_integer_to_word8 :: Integer -> P.Result
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

prop_double_to_word8 :: Double -> P.Result
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

propIntDouble :: Int -> P.Result
propIntDouble x =
    safeConvert x @?= Right ((fromIntegral x)::Double)

propIntChar :: Int -> P.Result
propIntChar x =
    safeConvert x @?= if x >= fromEnum (minBound :: Char) &&
                         x <= fromEnum (maxBound :: Char)
                      then Right ((toEnum x)::Char)
                      else Left $ ConvertError (show x) "Int" "Char" "Input value outside of bounds: ('\\NUL','\\1114111')"

propCharInt :: Int -> Property
propCharInt x =
    x >= fromEnum (minBound :: Char) && x <= fromEnum (maxBound :: Char) ==>
    safeConvert c @?= Right ((fromEnum c)::Int)
    where c = (toEnum x)::Char

propIntIntegerInt :: Int -> P.Result
propIntIntegerInt x =
    Right x @=? do r1 <- ((safeConvert x)::ConvertResult Integer)
                   ((safeConvert r1)::ConvertResult Int)
    
propDoubleRationalDouble :: Double -> P.Result
propDoubleRationalDouble x =
    Right x @=? do r1 <- ((safeConvert x)::ConvertResult Rational)
                   ((safeConvert r1)::ConvertResult Double)

allt = [q "Int -> Integer" prop_int_to_integer,
        q "Integer -> Int (safe bounds)" prop_integer_to_int_pass,
        q "Integer -> Word8 (general)" prop_integer_to_word8,
        q "Integer -> Word8 (safe bounds)" prop_integer_to_word8_safe,
        q "Integer -> Word8 (unsafe bounds)" prop_integer_to_word8_unsafe,
        q "Double -> Word8 (general)" prop_double_to_word8,
        q "Double -> Word8 (safe bounds)" prop_double_to_word8_safe,
        q "Double -> Word8 (unsafe bounds)" prop_double_to_word8_unsafe,
        q "Int -> Double" propIntDouble,
        q "Int -> Char" propIntChar,
        q "Char -> Int" propCharInt,
        q "identity Int -> Integer -> Int" propIntIntegerInt,
        q "identity Double -> Rational -> Double" propDoubleRationalDouble
       ]
