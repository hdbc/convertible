{-
Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module TestTime (testTime) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
--import Test.QuickCheck.Tools
--import Test.QuickCheck.Instances

import Data.Convertible
import qualified System.Time as ST
import Data.Time
import Data.Time.Clock.POSIX
import Data.Ratio
import Foreign.C.Types

instance Arbitrary ST.ClockTime where
    arbitrary = do r1 <- arbitrary
                   r2 <- sized $ \n -> choose (0, 1000000000000 - 1)
                   return (ST.TOD r1 r2)
    -- coarbitrary (ST.TOD a b) = coarbitrary a . coarbitrary b

instance Arbitrary ST.CalendarTime where
    arbitrary = do r <- arbitrary
                   return $ convert (r::POSIXTime)

instance Arbitrary NominalDiffTime where
    arbitrary = do r <- arbitrary
                   return $ convert (r::ST.ClockTime)

instance Arbitrary UTCTime where
    arbitrary = do r <- arbitrary
                   return $ convert (r::POSIXTime)

instance Arbitrary ZonedTime where
    arbitrary = do r <- arbitrary
                   return $ convert (r::POSIXTime)

instance Eq ZonedTime where
    a == b = zonedTimeToUTC a == zonedTimeToUTC b

propCltCalt :: ST.ClockTime -> Bool
propCltCalt x =
    safeConvert x == Right (ST.toUTCTime x)

propCltCaltClt :: ST.ClockTime -> Bool
propCltCaltClt x =
    Right x == do r1 <- ((safeConvert x)::ConvertResult ST.CalendarTime)
                  safeConvert r1

propCltPT :: ST.ClockTime -> Bool
propCltPT x@(ST.TOD y z) =
    safeConvert x == Right (r::POSIXTime)
    where r = fromRational $ fromInteger y + fromRational (z % 1000000000000)

propPTClt :: POSIXTime -> Bool
propPTClt x =
    safeConvert x == Right (r::ST.ClockTime)
    where r = ST.TOD rsecs rpico
          rsecs = floor x
          rpico = truncate $ abs $ 1000000000000 * (x - (fromIntegral rsecs))

propCaltPT :: ST.CalendarTime -> Bool
propCaltPT x =
    safeConvert x == expected
        where expected = do r <- safeConvert x
                            (safeConvert (r :: ST.ClockTime))::ConvertResult POSIXTime

propCltPTClt :: ST.ClockTime -> Bool
propCltPTClt x =
    Right (toTOD x) == case do r1 <- (safeConvert x)::ConvertResult POSIXTime
                               safeConvert r1
                       of Left x -> Left x
                          Right y -> Right $ toTOD y
    where toTOD (ST.TOD x y) = (x, y)
{-
    Right x @=? do r1 <- (safeConvert x)::ConvertResult POSIXTime
                   safeConvert r1
-}

propPTZTPT :: POSIXTime -> Bool
propPTZTPT x =
    Right x == do r1 <- safeConvert x
                  safeConvert (r1 :: ZonedTime)

propPTCltPT :: POSIXTime -> Bool
propPTCltPT x =
    Right x == do r1 <- (safeConvert x)::ConvertResult ST.ClockTime
                  safeConvert r1

propPTCalPT :: POSIXTime -> Bool
propPTCalPT x =
    Right x == do r1 <- safeConvert x
                  safeConvert (r1::ST.CalendarTime)

propUTCCaltUTC :: UTCTime -> Bool
propUTCCaltUTC x =
    Right x == do r1 <- safeConvert x
                  safeConvert (r1::ST.CalendarTime)

propPTUTC :: POSIXTime -> Bool
propPTUTC x =
    safeConvert x == Right (posixSecondsToUTCTime x)

propUTCPT :: UTCTime -> Bool
propUTCPT x =
    safeConvert x == Right (utcTimeToPOSIXSeconds x)

propCltUTC :: ST.ClockTime -> Bool
propCltUTC x =
    safeConvert x == Right (posixSecondsToUTCTime . convert $ x)

propZTCTeqZTCaltCt :: ZonedTime -> Bool
propZTCTeqZTCaltCt x =
    route1 == route2
    where route1 = (safeConvert x)::ConvertResult ST.ClockTime
          route2 = do calt <- safeConvert x
                      safeConvert (calt :: ST.CalendarTime)

propCaltZTCalt :: ST.ClockTime -> Bool
propCaltZTCalt x =
    Right x == do zt <- ((safeConvert calt)::ConvertResult ZonedTime)
                  calt' <- ((safeConvert zt)::ConvertResult ST.CalendarTime)
                  return (ST.toClockTime calt')
    where calt = ST.toUTCTime x

propCaltZTCalt2 :: ST.CalendarTime -> Bool
propCaltZTCalt2 x =
    Right x == do zt <- safeConvert x
                  safeConvert (zt :: ZonedTime)

propZTCaltCtZT :: ZonedTime -> Bool
propZTCaltCtZT x =
    Right x == do calt <- safeConvert x
                  ct <- safeConvert (calt :: ST.CalendarTime)
                  safeConvert (ct :: ST.ClockTime)

propZTCtCaltZT :: ZonedTime -> Bool
propZTCtCaltZT x =
    Right x == do ct <- safeConvert x
                  calt <- safeConvert (ct :: ST.ClockTime)
                  safeConvert (calt :: ST.CalendarTime)

propZTCaltZT :: ZonedTime -> Bool
propZTCaltZT x =
    Right x == do calt <- safeConvert x
                  safeConvert (calt :: ST.CalendarTime)

propZTCtCaltCtZT :: ZonedTime -> Bool
propZTCtCaltCtZT x =
    Right x == do ct <- safeConvert x
                  calt <- safeConvert (ct :: ST.ClockTime)
                  ct' <- safeConvert (calt :: ST.CalendarTime)
                  safeConvert (ct' :: ST.ClockTime)

propUTCZT :: UTCTime -> Bool
propUTCZT x =
          x == zonedTimeToUTC (convert x)

propUTCZTUTC :: UTCTime -> Bool
propUTCZTUTC x =
    Right x == do r1 <- ((safeConvert x)::ConvertResult ZonedTime)
                  safeConvert r1

propNdtTdNdt :: NominalDiffTime -> Bool
propNdtTdNdt x =
    Right x == do r1 <- ((safeConvert x)::ConvertResult ST.TimeDiff)
                  safeConvert r1

propPTCPT :: POSIXTime -> Bool
propPTCPT x =
    Right testval == do r1 <- safeConvert testval
                        safeConvert (r1 :: CTime)
        where testval = (convert ((truncate x)::Integer))::POSIXTime      -- CTime doesn't support picosecs

testTime = testGroup "TestTime"
  [ testProperty "ClockTime -> CalendarTime" propCltCalt
  , testProperty "ClockTime -> CalendarTime -> ClockTime" propCltCaltClt
  , testProperty "ClockTime -> POSIXTime" propCltPT
  , testProperty "POSIXTime -> ClockTime" propPTClt
  , testProperty "CalendarTime -> POSIXTime" propCaltPT
  , testProperty "identity ClockTime -> POSIXTime -> ClockTime" propCltPTClt
  , testProperty "identity POSIXTime -> ClockTime -> POSIXTime" propPTCltPT
  , testProperty "identity POSIXTime -> ZonedTime -> POSIXTime" propPTZTPT
  , testProperty "identity POSIXTime -> CalendarTime -> POSIXTime" propPTCalPT
  , testProperty "identity UTCTime -> CalendarTime -> UTCTime" propUTCCaltUTC
  , testProperty "POSIXTime -> UTCTime" propPTUTC
  , testProperty "UTCTime -> POSIXTime" propUTCPT
  , testProperty "ClockTime -> UTCTime" propCltUTC
  , testProperty "ZonedTime -> ClockTime == ZonedTime -> CalendarTime -> ClockTime" propZTCTeqZTCaltCt
  , testProperty "identity CalendarTime -> ZonedTime -> CalendarTime" propCaltZTCalt
  , testProperty "identity CalendarTime -> ZonedTime -> CalenderTime, test 2" propCaltZTCalt2
  , testProperty "identity ZonedTime -> CalendarTime -> ZonedTime" propZTCaltZT
  , testProperty "ZonedTime -> CalendarTime -> ClockTime -> ZonedTime" propZTCaltCtZT
  , testProperty "ZonedTime -> ClockTime -> CalendarTime -> ZonedTime" propZTCtCaltZT
  , testProperty "ZonedTime -> ColckTime -> CalendarTime -> ClockTime -> ZonedTime" propZTCtCaltCtZT
  , testProperty "UTCTime -> ZonedTime" propUTCZT
  , testProperty "UTCTime -> ZonedTime -> UTCTime" propUTCZTUTC
  , testProperty "identity NominalDiffTime -> TimeDiff -> NominalDiffTime" propNdtTdNdt
  , testProperty "identity POSIXTime -> CTime -> POSIXTime" propPTCPT
  ]
