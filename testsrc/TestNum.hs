{-
Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}
module TestNum where

import Data.Convertible

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Assertions
import Test.QuickCheck.Property
import Data.Decimal
import Control.Applicative
  
import Data.Word
import Data.Int

#if MIN_VERSION_Decimal(0,2,4)
-- Decimal-0.2.4 has no Arbitrary instance in library any more
instance (Arbitrary i, Integral i) => Arbitrary (DecimalRaw i) where
  arbitrary = Decimal <$> arbitrary <*> arbitrary
#endif

prop_int_to_integer :: Int -> Result
prop_int_to_integer x =
    safeConvert x ?== Right ((fromIntegral x)::Integer)

prop_integer_to_int_pass :: Integer -> Property
prop_integer_to_int_pass x =
    (x <= fromIntegral (maxBound :: Int)) &&
    (x >= fromIntegral (minBound :: Int)) ==> 
                                          safeConvert x ?== Right ((fromIntegral x)::Int)

prop_integer_to_word8 :: Integer -> Result
prop_integer_to_word8 x =
    safeConvert x ?== if x >= fromIntegral (minBound :: Word8) &&
                         x <= fromIntegral (maxBound :: Word8)
                      then Right ((fromIntegral x)::Word8)
                      else Left $ ConvertError (show x) "Integer" "Word8" "Input value outside of bounds: (0,255)"

prop_integer_to_word8_safe :: Integer -> Property
prop_integer_to_word8_safe x =
    x <= fromIntegral (maxBound :: Word8) &&
    x >= fromIntegral (minBound :: Word8) ==>
      safeConvert x ?== Right ((fromIntegral x)::Word8)

prop_integer_to_word8_unsafe :: Integer -> Property
prop_integer_to_word8_unsafe x =
    x < fromIntegral (minBound :: Word8) ||
    x > fromIntegral (maxBound :: Word8) ==>
      ((safeConvert x)::ConvertResult Word8) ?== (Left $ ConvertError (show x) "Integer" "Word8" "Input value outside of bounds: (0,255)")

prop_double_to_word8 :: Double -> Result
prop_double_to_word8 x =
    safeConvert x ?== if truncate x >= toInteger (minBound :: Word8) &&
                         truncate x <= toInteger (maxBound :: Word8)
                      then Right ((truncate x)::Word8)
                      else Left $ ConvertError (show x) "Double" "Word8" "Input value outside of bounds: (0,255)"

prop_double_to_word8_safe :: Double -> Property
prop_double_to_word8_safe x =
    x <= fromIntegral (maxBound :: Word8) &&
    x >= fromIntegral (minBound :: Word8) ==>
      safeConvert x ?== Right ((truncate x)::Word8)

prop_double_to_word8_unsafe :: Double -> Property
prop_double_to_word8_unsafe x =
    truncate x < toInteger (minBound :: Word8) ||
    truncate x > toInteger (maxBound :: Word8) ==>
      ((safeConvert x)::ConvertResult Word8) ?== (Left $ ConvertError (show x) "Double" "Word8" "Input value outside of bounds: (0,255)")

propIntDouble :: Int -> Result
propIntDouble x =
    safeConvert x ?== Right ((fromIntegral x)::Double)

propIntChar :: Int -> Result
propIntChar x =
    safeConvert x ?== if x >= fromEnum (minBound :: Char) &&
                         x <= fromEnum (maxBound :: Char)
                      then Right ((toEnum x)::Char)
                      else Left $ ConvertError (show x) "Int" "Char" "Input value outside of bounds: ('\\NUL','\\1114111')"

propCharInt :: Char -> Result
propCharInt c =
    safeConvert c ?== Right ((fromEnum c)::Int)
    where x = fromEnum c

propIntIntegerInt :: Int -> Result
propIntIntegerInt x =
    Right x ==? do r1 <- ((safeConvert x)::ConvertResult Integer)
                   ((safeConvert r1)::ConvertResult Int)
    
propDoubleRationalDouble :: Double -> Result
propDoubleRationalDouble x =
    Right x ==? do r1 <- ((safeConvert x)::ConvertResult Rational)
                   ((safeConvert r1)::ConvertResult Double)

propFloatToDouble :: Float -> Result
propFloatToDouble x = x ~==? (convert (convert x :: Double))

propDoubleToDecimal :: Double -> Result
propDoubleToDecimal x = x ~==? (convert (convert x :: Decimal))

propDecimalToRational :: Decimal -> Result
propDecimalToRational x = x ==? (convert (convert x :: Rational))

propInt32ToDecimal :: Int32 -> Result
propInt32ToDecimal x = x ==? (convert (convert x :: Decimal))


allt = describe "Numeric tests" $ do
  prop "Int -> Integer" prop_int_to_integer
  prop "Integer -> Int (safe bounds)" prop_integer_to_int_pass
  prop "Integer -> Word8 (general)" prop_integer_to_word8
  prop "Integer -> Word8 (safe bounds)" prop_integer_to_word8_safe
  prop "Integer -> Word8 (unsafe bounds)" prop_integer_to_word8_unsafe
  prop "Double -> Word8 (general)" prop_double_to_word8
  prop "Double -> Word8 (safe bounds)" prop_double_to_word8_safe
  prop "Double -> Word8 (unsafe bounds)" prop_double_to_word8_unsafe
  prop "Int -> Double" propIntDouble
  prop "Int -> Char" propIntChar
  prop "Char -> Int" propCharInt
  prop "identity Int -> Integer -> Int" propIntIntegerInt
  prop "identity Double -> Rational -> Double" propDoubleRationalDouble
  prop "Float -> Double -> Float" propFloatToDouble
  prop "Doublde -> Decimal -> Double" propDoubleToDecimal
  prop "Decimal -> Rational -> Decimal" propDecimalToRational
  prop "Int32 -> Decimal -> Int32" propInt32ToDecimal
