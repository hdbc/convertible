{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

module TestNum where
import TestInfrastructure
import Data.Convertible
import Test.QuickCheck
import Test.QuickCheck.Utils

prop_int_to_integer :: Int -> Result
prop_int_to_integer x =
    safeConvert x @?= Right ((fromIntegral x)::Integer)

allt = [q "int -> integer" prop_int_to_integer]
