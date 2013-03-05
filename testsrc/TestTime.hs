{-# LANGUAGE
    ScopedTypeVariables #-}

{-
Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module TestTime where

import Data.Convertible

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (arbitrary, Gen)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Assertions
import Test.QuickCheck.Property
  
import qualified System.Time as ST
import Data.Time
import Data.Time.Clock.POSIX
import Data.Ratio
import Data.AEq
import Foreign.C.Types

genZoned :: Gen ZonedTime
genZoned = fmap convert (arbitrary :: Gen POSIXTime)

instance Eq ZonedTime where
    a == b = zonedTimeToUTC a == zonedTimeToUTC b

propCltCalt :: ST.ClockTime -> Result
propCltCalt x =
    safeConvert x ?== Right (ST.toUTCTime x)

propCltCaltClt :: ST.ClockTime -> Result
propCltCaltClt x =
    Right x ==? do r1 <- ((safeConvert x)::ConvertResult ST.CalendarTime)
                   safeConvert r1

propCltPT :: ST.ClockTime -> Result
propCltPT x@(ST.TOD y z) =
    safeConvert x ?== Right (r::POSIXTime)
    where r = fromRational $ fromInteger y + fromRational (z % 1000000000000)

propPTClt :: POSIXTime -> Result
propPTClt x =
    safeConvert x ?== Right (r::ST.ClockTime)
    where r = ST.TOD rsecs rpico
          rsecs = floor x
          rpico = truncate $ abs $ 1000000000000 * (x - (fromIntegral rsecs))

propCaltPT :: ST.CalendarTime -> Result
propCaltPT x =
    safeConvert x ?== expected
        where expected = do r <- safeConvert x
                            (safeConvert (r :: ST.ClockTime))::ConvertResult POSIXTime

propCltPTClt :: ST.ClockTime -> Result
propCltPTClt x =
    Right (toTOD x) ==? case do r1 <- (safeConvert x)::ConvertResult POSIXTime
                                safeConvert r1
                        of Left z -> Left z
                           Right y -> Right $ toTOD y
    where toTOD (ST.TOD z y) = (z, y)
{-
    Right x ==? do r1 <- (safeConvert x)::ConvertResult POSIXTime
                   safeConvert r1
-}

propPTZTPT :: POSIXTime -> Result
propPTZTPT x =
    Right x ==? do r1 <- safeConvert x
                   safeConvert (r1 :: ZonedTime)

propPTCltPT :: POSIXTime -> Result
propPTCltPT x =
    Right x ==? do r1 <- (safeConvert x)::ConvertResult ST.ClockTime
                   safeConvert r1

propPTCalPT :: POSIXTime -> Result
propPTCalPT x =
    Right x ==? do r1 <- safeConvert x
                   safeConvert (r1::ST.CalendarTime)

propUTCCaltUTC :: UTCTime -> Result
propUTCCaltUTC x =
    Right x ==? do r1 <- safeConvert x
                   safeConvert (r1::ST.CalendarTime)

propPTUTC :: POSIXTime -> Result
propPTUTC x =
    safeConvert x ?== Right (posixSecondsToUTCTime x)
propUTCPT :: UTCTime -> Result
propUTCPT x =
    safeConvert x ?== Right (utcTimeToPOSIXSeconds x)

propCltUTC :: ST.ClockTime -> Result
propCltUTC x =
    safeConvert x ?== Right (posixSecondsToUTCTime . convert $ x)

propZTCTeqZTCaltCt :: ZonedTime -> Result
propZTCTeqZTCaltCt x =
    route1 ==? route2
    where route1 = (safeConvert x)::ConvertResult ST.ClockTime
          route2 = do calt <- safeConvert x
                      safeConvert (calt :: ST.CalendarTime)

propCaltZTCalt :: ST.ClockTime -> Result
propCaltZTCalt x =
    Right x ==? do zt <- ((safeConvert calt)::ConvertResult ZonedTime)
                   calt' <- ((safeConvert zt)::ConvertResult ST.CalendarTime)
                   return (ST.toClockTime calt')
    where calt = ST.toUTCTime x

propCaltZTCalt2 :: ST.CalendarTime -> Result
propCaltZTCalt2 x =
    Right x ==? do zt <- safeConvert x
                   safeConvert (zt :: ZonedTime)

propZTCaltCtZT :: ZonedTime -> Result
propZTCaltCtZT x =
    Right x ==? do calt <- safeConvert x
                   ct <- safeConvert (calt :: ST.CalendarTime)
                   safeConvert (ct :: ST.ClockTime)

propZTCtCaltZT :: ZonedTime -> Result
propZTCtCaltZT x =
    Right x ==? do ct <- safeConvert x
                   calt <- safeConvert (ct :: ST.ClockTime)
                   safeConvert (calt :: ST.CalendarTime)

propZTCaltZT :: ZonedTime -> Result
propZTCaltZT x =
    Right x ==? do calt <- safeConvert x
                   safeConvert (calt :: ST.CalendarTime)

propZTCtCaltCtZT :: ZonedTime -> Result
propZTCtCaltCtZT x =
    Right x ==? do ct <- safeConvert x
                   calt <- safeConvert (ct :: ST.ClockTime)
                   ct' <- safeConvert (calt :: ST.CalendarTime)
                   safeConvert (ct' :: ST.ClockTime)

propUTCZT :: UTCTime -> Bool
propUTCZT x =
          x == zonedTimeToUTC (convert x)

propUTCZTUTC :: UTCTime -> Result
propUTCZTUTC x =
    Right x ==? do r1 <- ((safeConvert x)::ConvertResult ZonedTime)
                   safeConvert r1

propNdtTdNdt :: NominalDiffTime -> Result
propNdtTdNdt x =
    Right x ==? do r1 <- ((safeConvert x)::ConvertResult ST.TimeDiff)
                   safeConvert r1

propPTCPT :: POSIXTime -> Result
propPTCPT x =
    Right testval ==? do r1 <- safeConvert testval
                         safeConvert (r1 :: CTime)
        where testval = (convert ((truncate x)::Integer))::POSIXTime      -- CTime doesn't support picosecs

propFromTo :: (Convertible a b, Convertible b a, Eq a, Show a) => a -> b -> Result
propFromTo a b = a ==? (convert (asTypeOf (convert a) b))

propFromToAlmost :: (Convertible a b, Convertible b a, AEq a, Show a) => a -> b -> Result
propFromToAlmost a b = a ~==? (convert (asTypeOf (convert a) b))

              
allt :: Spec
allt = describe "Date and time tests" $ do
  prop "ClockTime -> CalendarTime" propCltCalt
  prop "ClockTime -> CalendarTime -> ClockTime" propCltCaltClt
  prop "ClockTime -> POSIXTime" propCltPT
  prop "POSIXTime -> ClockTime" propPTClt
  prop "CalendarTime -> POSIXTime" propCaltPT
  prop "identity ClockTime -> POSIXTime -> ClockTime" propCltPTClt
  prop "identity POSIXTime -> ClockTime -> POSIXTime" propPTCltPT
  prop "identity POSIXTime -> ZonedTime -> POSIXTime" propPTZTPT
  prop "identity POSIXTime -> CalendarTime -> POSIXTime" propPTCalPT
  prop "identity UTCTime -> CalendarTime -> UTCTime" propUTCCaltUTC
  prop "POSIXTime -> UTCTime" propPTUTC
  prop "UTCTime -> POSIXTime" propUTCPT
  prop "ClockTime -> UTCTime" propCltUTC
  prop "ZonedTime -> ClockTime == ZonedTime -> CalendarTime -> ClockTime" $ forAll genZoned propZTCTeqZTCaltCt
  prop "identity CalendarTime -> ZonedTime -> CalendarTime" propCaltZTCalt
  prop "identity CalendarTime -> ZonedTime -> CalenderTime, test 2" propCaltZTCalt2
  prop "identity ZonedTime -> CalendarTime -> ZonedTime" propZTCaltZT
  prop "ZonedTime -> CalendarTime -> ClockTime -> ZonedTime" $ forAll genZoned propZTCaltCtZT
  prop "ZonedTime -> ClockTime -> CalendarTime -> ZonedTime" propZTCtCaltZT
  prop "ZonedTime -> ColckTime -> CalendarTime -> ClockTime -> ZonedTime" propZTCtCaltCtZT
  prop "UTCTime -> ZonedTime" propUTCZT
  prop "UTCTime -> ZonedTime -> UTCTime" propUTCZTUTC
  prop "identity NominalDiffTime -> TimeDiff -> NominalDiffTime" propNdtTdNdt
  prop "identity POSIXTime -> CTime -> POSIXTime" propPTCPT
  prop "Integer -> ST.ClockTime -> Integer" $ \(i :: Integer) -> propFromTo i (undefined :: ST.ClockTime)
  prop "POSIXTime -> Rational -> POSIXTime" $ \(i :: POSIXTime) -> propFromTo i (undefined :: Rational)
  prop "Double -> POSIXTime -> Double" $ \(i :: Double) -> propFromToAlmost i (undefined :: POSIXTime)
  prop "Int -> POSIXTime -> Int" $ \(i :: Int) -> propFromTo i (undefined :: POSIXTime)
  prop "UTCTime -> Rational -> UTCTime" $ \(i :: UTCTime) -> propFromTo i (undefined :: Rational)
  prop "Integer -> UTCTime -> Integer" $ \(i :: Integer) -> propFromTo i (undefined :: UTCTime)
  prop "Int -> UTCTime -> Int" $ \(i :: Int) -> propFromTo i (undefined :: UTCTime)
  prop "Double -> UTCTime -> Double" $ \(i :: Double) -> propFromToAlmost i (undefined :: UTCTime)
  prop "ST.CalendarTime -> POSIXTime -> ST.CalendarTime" $ \(i :: ST.CalendarTime) -> propFromTo i (undefined :: POSIXTime)
  prop "UTCTime -> ST.ClockTime -> UTCTime" $ \(i :: UTCTime) -> propFromTo i (undefined :: ST.ClockTime)
  prop "Integer -> ST.TimeDiff -> Integer" $ \(i :: Integer) -> propFromTo i (undefined :: ST.TimeDiff)
  prop "Double -> ST.TimeDiff -> Double" $ \(i :: Double) -> propFromToAlmost i (undefined :: ST.TimeDiff)
  
