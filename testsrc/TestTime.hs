{-
Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module TestTime where
import Data.Convertible
import Test.QuickCheck ((===), Arbitrary, Property, Testable, arbitrary, choose, quickCheck, sized)
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

propCltCalt :: ST.ClockTime -> Property
propCltCalt x =
    safeConvert x === Right (ST.toUTCTime x)

propCltCaltClt :: ST.ClockTime -> Property
propCltCaltClt x =
    Right x === do r1 <- safeConvert x :: ConvertResult ST.CalendarTime
                   safeConvert r1

propCltPT :: ST.ClockTime -> Property
propCltPT x@(ST.TOD y z) =
    safeConvert x === Right (r::POSIXTime)
    where r = fromRational $ fromInteger y + fromRational (z % 1000000000000)

propPTClt :: POSIXTime -> Property
propPTClt x =
    safeConvert x === Right (r::ST.ClockTime)
    where r = ST.TOD rsecs rpico
          rsecs = floor x
          rpico = truncate $ abs $ 1000000000000 * (x - fromIntegral rsecs)

propCaltPT :: ST.CalendarTime -> Property
propCaltPT x =
    safeConvert x === expected
        where expected = do r <- safeConvert x
                            safeConvert (r :: ST.ClockTime) :: ConvertResult POSIXTime

propCltPTClt :: ST.ClockTime -> Property
propCltPTClt x =
    Right (toTOD x) === case do r1 <- safeConvert x :: ConvertResult POSIXTime
                                safeConvert r1
                        of Left x -> Left x
                           Right y -> Right $ toTOD y
    where toTOD (ST.TOD x y) = (x, y)
{-
    Right x @=? do r1 <- (safeConvert x)::ConvertResult POSIXTime
                   safeConvert r1
-}

propPTZTPT :: POSIXTime -> Property
propPTZTPT x =
    Right x === do r1 <- safeConvert x
                   safeConvert (r1 :: ZonedTime)

propPTCltPT :: POSIXTime -> Property
propPTCltPT x =
    Right x === do r1 <- safeConvert x :: ConvertResult ST.ClockTime
                   safeConvert r1

propPTCalPT :: POSIXTime -> Property
propPTCalPT x =
    Right x === do r1 <- safeConvert x
                   safeConvert (r1::ST.CalendarTime)

propUTCCaltUTC :: UTCTime -> Property
propUTCCaltUTC x =
    Right x === do r1 <- safeConvert x
                   safeConvert (r1::ST.CalendarTime)

propPTUTC :: POSIXTime -> Property
propPTUTC x =
    safeConvert x === Right (posixSecondsToUTCTime x)
propUTCPT :: UTCTime -> Property
propUTCPT x =
    safeConvert x === Right (utcTimeToPOSIXSeconds x)

propCltUTC :: ST.ClockTime -> Property
propCltUTC x =
    safeConvert x === Right (posixSecondsToUTCTime . convert $ x)

propZTCTeqZTCaltCt :: ZonedTime -> Property
propZTCTeqZTCaltCt x =
    route1 === route2
    where route1 = safeConvert x :: ConvertResult ST.ClockTime
          route2 = do calt <- safeConvert x
                      safeConvert (calt :: ST.CalendarTime)

propCaltZTCalt :: ST.ClockTime -> Property
propCaltZTCalt x =
    Right x === do zt <- safeConvert calt :: ConvertResult ZonedTime
                   calt' <- safeConvert zt :: ConvertResult ST.CalendarTime
                   return (ST.toClockTime calt')
    where calt = ST.toUTCTime x

propCaltZTCalt2 :: ST.CalendarTime -> Property
propCaltZTCalt2 x =
    Right x === do zt <- safeConvert x
                   safeConvert (zt :: ZonedTime)

propZTCaltCtZT :: ZonedTime -> Property
propZTCaltCtZT x =
    Right x === do calt <- safeConvert x
                   ct <- safeConvert (calt :: ST.CalendarTime)
                   safeConvert (ct :: ST.ClockTime)

propZTCtCaltZT :: ZonedTime -> Property
propZTCtCaltZT x =
    Right x === do ct <- safeConvert x
                   calt <- safeConvert (ct :: ST.ClockTime)
                   safeConvert (calt :: ST.CalendarTime)

propZTCaltZT :: ZonedTime -> Property
propZTCaltZT x =
    Right x === do calt <- safeConvert x
                   safeConvert (calt :: ST.CalendarTime)

propZTCtCaltCtZT :: ZonedTime -> Property
propZTCtCaltCtZT x =
    Right x === do ct <- safeConvert x
                   calt <- safeConvert (ct :: ST.ClockTime)
                   ct' <- safeConvert (calt :: ST.CalendarTime)
                   safeConvert (ct' :: ST.ClockTime)

propUTCZT :: UTCTime -> Bool
propUTCZT x =
          x == zonedTimeToUTC (convert x)

propUTCZTUTC :: UTCTime -> Property
propUTCZTUTC x =
    Right x === do r1 <- safeConvert x :: ConvertResult ZonedTime
                   safeConvert r1

propNdtTdNdt :: NominalDiffTime -> Property
propNdtTdNdt x =
    Right x === do r1 <- safeConvert x :: ConvertResult ST.TimeDiff
                   safeConvert r1

propPTCPT :: POSIXTime -> Property
propPTCPT x =
    Right testval === do r1 <- safeConvert testval
                         safeConvert (r1 :: CTime)
        where
          testval :: POSIXTime
          testval = convert (truncate x :: Integer) -- CTime doesn't support picosecs

q :: Testable prop => String -> prop -> IO ()
q testLabel prop = do
  putStrLn testLabel
  quickCheck prop

allt :: [IO ()]
allt = [q "ClockTime -> CalendarTime" propCltCalt,
        q "ClockTime -> CalendarTime -> ClockTime" propCltCaltClt,
        q "ClockTime -> POSIXTime" propCltPT,
        q "POSIXTime -> ClockTime" propPTClt,
        q "CalendarTime -> POSIXTime" propCaltPT,
        q "identity ClockTime -> POSIXTime -> ClockTime" propCltPTClt,
        q "identity POSIXTime -> ClockTime -> POSIXTime" propPTCltPT,
        q "identity POSIXTime -> ZonedTime -> POSIXTime" propPTZTPT,
        q "identity POSIXTime -> CalendarTime -> POSIXTime" propPTCalPT,
        q "identity UTCTime -> CalendarTime -> UTCTime" propUTCCaltUTC,
        q "POSIXTime -> UTCTime" propPTUTC,
        q "UTCTime -> POSIXTime" propUTCPT,
        q "ClockTime -> UTCTime" propCltUTC,
        q "ZonedTime -> ClockTime == ZonedTime -> CalendarTime -> ClockTime" propZTCTeqZTCaltCt,
        q "identity CalendarTime -> ZonedTime -> CalendarTime" propCaltZTCalt,
        q "identity CalendarTime -> ZonedTime -> CalenderTime, test 2" propCaltZTCalt2,
        q "identity ZonedTime -> CalendarTime -> ZonedTime" propZTCaltZT,
        q "ZonedTime -> CalendarTime -> ClockTime -> ZonedTime" propZTCaltCtZT,
        q "ZonedTime -> ClockTime -> CalendarTime -> ZonedTime" propZTCtCaltZT,
        q "ZonedTime -> ColckTime -> CalendarTime -> ClockTime -> ZonedTime" propZTCtCaltCtZT,
        q "UTCTime -> ZonedTime" propUTCZT,
        q "UTCTime -> ZonedTime -> UTCTime" propUTCZTUTC,
        q "identity NominalDiffTime -> TimeDiff -> NominalDiffTime" propNdtTdNdt,
        q "identity POSIXTime -> CTime -> POSIXTime" propPTCPT
       ]
