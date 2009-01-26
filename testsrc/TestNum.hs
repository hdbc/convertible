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
    safeConvert x @?= Right ((fromIntegral x)::Word8)

allt = [q "int -> integer" prop_int_to_integer,
        q "integer -> int" prop_integer_to_int_pass,
        q "integer -> word8" prop_integer_to_word8]
