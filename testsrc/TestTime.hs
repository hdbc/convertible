{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}

module TestTime where
import TestInfrastructure
import Data.Convertible
import Test.QuickCheck
import Test.QuickCheck.Tools
import Test.QuickCheck.Instances
import qualified System.Time as ST
import Data.Time

instance Arbitrary ST.ClockTime where
    arbitrary = do r1 <- arbitrary
                   r2 <- sized $ \n -> choose (0, 1000000000000 - 1)
                   return (ST.TOD r1 r2)
    coarbitrary (ST.TOD a b) = coarbitrary a . coarbitrary b

propCltCalt :: ST.ClockTime -> Result
propCltCalt x =
    safeConvert x @?= Right (ST.toUTCTime x)

propCltCaltClt :: ST.ClockTime -> Result
propCltCaltClt x =
    Right x @=? do r1 <- ((safeConvert x)::ConvertResult ST.CalendarTime)
                   safeConvert r1

allt = [q "ClockTime -> CalendarTime" propCltCalt,
        q "ClockTime -> CalendarTime -> ClockTime" propCltCaltClt]
