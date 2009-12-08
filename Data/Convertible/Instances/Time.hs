{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{- |
   Module     : Data.ConvertAttempt.Instances.Time
   Copyright  : Copyright (C) 2009 John Goerzen
   License    : LGPL

   Maintainer : Michael Snoyman <michael@snoyman.com>
   Stability  : provisional
   Portability: portable

Instances to convert between various time structures, both old- and new-style.

At present, this module does not do full input validation.  That is, it is possible
to get an exception rather than a Left result from these functions if your input is
invalid, particularly when converting from the old-style System.Time structures.

Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

module Data.Convertible.Instances.Time()
where

import Data.Convertible.Base
import Data.Convertible.Utils
import Data.Convertible.Instances.Num()
import qualified System.Time as ST
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar.OrdinalDate
#ifndef TIME_GTE_113
import Data.Typeable
#endif
import Data.Ratio
import Foreign.C.Types

----------------------------------------------------------------------
-- Intra-System.Time stuff
----------------------------------------------------------------------

instance ConvertAttempt ST.ClockTime ST.CalendarTime where
    convertAttempt = return . ST.toUTCTime
instance ConvertSuccess ST.ClockTime ST.CalendarTime where
    convertSuccess = ST.toUTCTime

instance ConvertAttempt ST.CalendarTime ST.ClockTime where
    convertAttempt = return . ST.toClockTime
instance ConvertSuccess ST.CalendarTime ST.ClockTime where
    convertSuccess = ST.toClockTime

instance ConvertAttempt ST.ClockTime Integer where
    convertAttempt (ST.TOD x _) = return x
instance ConvertSuccess ST.ClockTime Integer where
    convertSuccess (ST.TOD x _) = x

instance ConvertAttempt Integer ST.ClockTime where
    convertAttempt x = return $ ST.TOD x 0
instance ConvertSuccess Integer ST.ClockTime where
    convertSuccess x = ST.TOD x 0

----------------------------------------------------------------------
-- Intra-Data.Time stuff
----------------------------------------------------------------------

------------------------------ POSIX and UTC times

#ifndef TIME_GTE_113
instance Typeable NominalDiffTime where
    typeOf _ = mkTypeName "NominalDiffTime"

instance Typeable UTCTime where
    typeOf _ = mkTypeName "UTCTime"
#endif

{- Covered under Real a
instance ConvertAttempt Rational POSIXTime where
    convertAttempt = return . fromRational
-}

instance ConvertSuccess Rational POSIXTime where
    convertSuccess = fromRational
instance ConvertSuccess Integer POSIXTime where
    convertSuccess = fromInteger
instance ConvertSuccess Int POSIXTime where
    convertSuccess = fromIntegral
instance ConvertSuccess Double POSIXTime where
    convertSuccess = realToFrac

instance ConvertAttempt Rational POSIXTime where
    convertAttempt = return . fromRational
instance ConvertAttempt Integer POSIXTime where
    convertAttempt = return . fromInteger
instance ConvertAttempt Int POSIXTime where
    convertAttempt = return . fromIntegral
instance ConvertAttempt Double POSIXTime where
    convertAttempt = return . realToFrac

instance ConvertSuccess POSIXTime Integer where
    convertSuccess = truncate
instance ConvertSuccess POSIXTime Rational where
    convertSuccess = toRational
instance ConvertSuccess POSIXTime Double where
    convertSuccess = realToFrac

instance ConvertAttempt POSIXTime Integer where
    convertAttempt = return . truncate
instance ConvertAttempt POSIXTime Rational where
    convertAttempt = return . toRational
instance ConvertAttempt POSIXTime Double where
    convertAttempt = return . realToFrac
instance ConvertAttempt POSIXTime Int where
    convertAttempt = boundedConversion (return . truncate)

instance ConvertSuccess POSIXTime UTCTime where
    convertSuccess = posixSecondsToUTCTime
instance ConvertSuccess UTCTime POSIXTime where
    convertSuccess = utcTimeToPOSIXSeconds

instance ConvertAttempt POSIXTime UTCTime where
    convertAttempt = return . posixSecondsToUTCTime
instance ConvertAttempt UTCTime POSIXTime where
    convertAttempt = return . utcTimeToPOSIXSeconds

instance ConvertSuccess Rational UTCTime where
    convertSuccess = posixSecondsToUTCTime . convertSuccess
instance ConvertSuccess Integer UTCTime where
    convertSuccess = posixSecondsToUTCTime . convertSuccess
instance ConvertSuccess Int UTCTime where
    convertSuccess = posixSecondsToUTCTime . convertSuccess
instance ConvertSuccess Double UTCTime where
    convertSuccess = posixSecondsToUTCTime . convertSuccess

instance ConvertAttempt Rational UTCTime where
    convertAttempt a = convertAttempt a >>= return . posixSecondsToUTCTime
instance ConvertAttempt Integer UTCTime where
    convertAttempt a = convertAttempt a >>= return . posixSecondsToUTCTime
instance ConvertAttempt Int UTCTime where
    convertAttempt a = convertAttempt a >>= return . posixSecondsToUTCTime
instance ConvertAttempt Double UTCTime where
    convertAttempt a = convertAttempt a >>= return . posixSecondsToUTCTime

instance ConvertSuccess UTCTime Rational where
    convertSuccess = convertSuccess . utcTimeToPOSIXSeconds
instance ConvertSuccess UTCTime Integer where
    convertSuccess = convertSuccess . utcTimeToPOSIXSeconds
instance ConvertSuccess UTCTime Double where
    convertSuccess = convertSuccess . utcTimeToPOSIXSeconds

instance ConvertAttempt UTCTime Rational where
    convertAttempt = convertAttempt . utcTimeToPOSIXSeconds
instance ConvertAttempt UTCTime Integer where
    convertAttempt = convertAttempt . utcTimeToPOSIXSeconds
instance ConvertAttempt UTCTime Double where
    convertAttempt = convertAttempt . utcTimeToPOSIXSeconds
instance ConvertAttempt UTCTime Int where
    convertAttempt = boundedConversion (convertAttempt . utcTimeToPOSIXSeconds)

------------------------------ LocalTime stuff

instance ConvertSuccess UTCTime ZonedTime where
    convertSuccess = utcToZonedTime utc
instance ConvertSuccess POSIXTime ZonedTime where
    convertSuccess = utcToZonedTime utc . posixSecondsToUTCTime
instance ConvertSuccess ZonedTime UTCTime where
    convertSuccess = zonedTimeToUTC
instance ConvertSuccess ZonedTime POSIXTime where
    convertSuccess = utcTimeToPOSIXSeconds . zonedTimeToUTC

instance ConvertAttempt UTCTime ZonedTime where
    convertAttempt = return . utcToZonedTime utc
instance ConvertAttempt POSIXTime ZonedTime where
    convertAttempt = return . utcToZonedTime utc . posixSecondsToUTCTime
instance ConvertAttempt ZonedTime UTCTime where
    convertAttempt = return . zonedTimeToUTC
instance ConvertAttempt ZonedTime POSIXTime where
    convertAttempt = return . utcTimeToPOSIXSeconds . zonedTimeToUTC

{- Too obvious?
instance ConvertAttempt LocalTime Day where
    convertAttempt = return . localDay
instance ConvertAttempt LocalTime TimeOfDay where
    convertAttempt = return . localTimeOfDay
-}

----------------------------------------------------------------------
-- Conversions between old and new time
----------------------------------------------------------------------
instance ConvertSuccess ST.CalendarTime ZonedTime where
    convertSuccess ct = ZonedTime {
     zonedTimeToLocalTime = LocalTime {
       localDay = fromGregorian (fromIntegral $ ST.ctYear ct) 
                  (1 + (fromEnum $ ST.ctMonth ct))
                  (ST.ctDay ct),
       localTimeOfDay = TimeOfDay {
         todHour = ST.ctHour ct,
         todMin = ST.ctMin ct,
         todSec = (fromIntegral $ ST.ctSec ct) + 
                  fromRational (ST.ctPicosec ct % 1000000000000)
                        }
                            },
     zonedTimeZone = TimeZone {
                       timeZoneMinutes = ST.ctTZ ct `div` 60,
                       timeZoneSummerOnly = ST.ctIsDST ct,
                       timeZoneName = ST.ctTZName ct}
}

instance ConvertSuccess ST.CalendarTime POSIXTime where
    convertSuccess = convertSuccessVia (undefined::ST.ClockTime)
instance ConvertSuccess ST.CalendarTime UTCTime where
    convertSuccess = convertSuccessVia (undefined::POSIXTime)

instance ConvertAttempt ST.CalendarTime POSIXTime where
    convertAttempt = convertAttemptVia (undefined::ST.ClockTime)
instance ConvertAttempt ST.CalendarTime UTCTime where
    convertAttempt = convertAttemptVia (undefined::POSIXTime)

instance ConvertSuccess ST.ClockTime POSIXTime where
    convertSuccess (ST.TOD x y) = fromRational $ 
                                        fromInteger x + fromRational (y % 1000000000000)

instance ConvertSuccess ST.ClockTime UTCTime where
    convertSuccess = convertSuccessVia (undefined::POSIXTime)
instance ConvertSuccess ST.ClockTime ZonedTime where
    convertSuccess = convertSuccessVia (undefined::UTCTime)
instance ConvertSuccess ZonedTime ST.ClockTime where
    convertSuccess = convertSuccessVia (undefined::POSIXTime)

instance ConvertAttempt ST.ClockTime UTCTime where
    convertAttempt = convertAttemptVia (undefined::POSIXTime)
instance ConvertAttempt ST.ClockTime ZonedTime where
    convertAttempt = convertAttemptVia (undefined::UTCTime)
instance ConvertAttempt ZonedTime ST.ClockTime where
    convertAttempt = convertAttemptVia (undefined::POSIXTime)

instance ConvertSuccess POSIXTime ST.ClockTime where
    convertSuccess x = ST.TOD rsecs rpico
        where rsecs = floor x
              rpico = truncate $ abs $ 1000000000000 * (x - (fromIntegral rsecs))

instance ConvertSuccess UTCTime ST.ClockTime where
    convertSuccess = convertSuccess . utcTimeToPOSIXSeconds
instance ConvertAttempt UTCTime ST.ClockTime where
    convertAttempt = convertAttempt . utcTimeToPOSIXSeconds

instance ConvertSuccess ZonedTime ST.CalendarTime where
    convertSuccess zt = ST.CalendarTime {
            ST.ctYear = fromIntegral year,
            ST.ctMonth = toEnum (month - 1),
            ST.ctDay = day,
            ST.ctHour = todHour ltod,
            ST.ctMin = todMin ltod,
            ST.ctSec = secs,
            ST.ctPicosec = pico,
            ST.ctWDay = toEnum . snd . sundayStartWeek . localDay . zonedTimeToLocalTime $ zt,
            ST.ctYDay = (snd . toOrdinalDate . localDay . zonedTimeToLocalTime $ zt) - 1,
            ST.ctTZName = timeZoneName . zonedTimeZone $ zt,
            ST.ctTZ = (timeZoneMinutes . zonedTimeZone $ zt) * 60,
            ST.ctIsDST = timeZoneSummerOnly . zonedTimeZone $ zt
          }
        where (year, month, day) = toGregorian . localDay . zonedTimeToLocalTime $ zt
              ltod = localTimeOfDay . zonedTimeToLocalTime $ zt
              secs = (truncate . todSec $ ltod)::Int
              picoRational = toRational (todSec ltod) - toRational secs
              pico = truncate (picoRational * 1000000000000)

instance ConvertSuccess POSIXTime ST.CalendarTime where
    convertSuccess = convertSuccessVia (undefined::ZonedTime)
instance ConvertSuccess UTCTime ST.CalendarTime where
    convertSuccess = convertSuccess . utcTimeToPOSIXSeconds

instance ConvertAttempt POSIXTime ST.CalendarTime where
    convertAttempt = convertAttemptVia (undefined::ZonedTime)
instance ConvertAttempt UTCTime ST.CalendarTime where
    convertAttempt = convertAttempt . utcTimeToPOSIXSeconds

instance ConvertSuccess ST.TimeDiff NominalDiffTime where
    {- This is a clever hack.  We convert the TimeDiff to a ClockTime, applying
       it as a diff vs. the epoch.  Converting this ClockTime to a POSIXTime yiels
       the NominalDiffTime we want, since a POSIXTime is a NominalDiffTime vs. the
       epoch. -}
    convertSuccess td = convertSuccess clockTime
        where clockTime = ST.addToClockTime td (ST.TOD 0 0)
instance ConvertSuccess NominalDiffTime ST.TimeDiff where
    {- Similar clever hack as above. -}
    convertSuccess ndt =
       let clockt = convertSuccess ndt
        in ST.diffClockTimes clockt (ST.TOD 0 0)

instance ConvertSuccess Integer ST.TimeDiff where
    convertSuccess = convertSuccessVia (undefined::NominalDiffTime)
instance ConvertSuccess Double ST.TimeDiff where
    convertSuccess = convertSuccessVia (undefined::NominalDiffTime)
instance ConvertSuccess ST.TimeDiff Integer where
    convertSuccess = convertSuccessVia (undefined :: NominalDiffTime)
instance ConvertSuccess ST.TimeDiff Rational where
    convertSuccess = convertSuccessVia (undefined :: NominalDiffTime)
instance ConvertSuccess ST.TimeDiff Double where
    convertSuccess = convertSuccessVia (undefined :: NominalDiffTime)

instance ConvertAttempt Integer ST.TimeDiff where
    convertAttempt = convertAttemptVia (undefined::NominalDiffTime)
instance ConvertAttempt Double ST.TimeDiff where
    convertAttempt = convertAttemptVia (undefined::NominalDiffTime)
instance ConvertAttempt ST.TimeDiff Integer where
    convertAttempt = convertAttemptVia (undefined :: NominalDiffTime)
instance ConvertAttempt ST.TimeDiff Rational where
    convertAttempt = convertAttemptVia (undefined :: NominalDiffTime)
instance ConvertAttempt ST.TimeDiff Double where
    convertAttempt = convertAttemptVia (undefined :: NominalDiffTime)

----------------------------------------------------------------------
-- Foreign.C Types
----------------------------------------------------------------------

instance ConvertSuccess CTime POSIXTime where
    convertSuccess = realToFrac
instance ConvertSuccess POSIXTime CTime where
    convertSuccess = fromInteger . truncate

instance ConvertSuccess CTime Integer where
    convertSuccess = truncate . toRational
instance ConvertSuccess Integer CTime where
    convertSuccess = fromInteger

instance ConvertSuccess CTime Double where
    convertSuccess = realToFrac
instance ConvertSuccess Double CTime where
    convertSuccess = fromInteger . truncate

instance ConvertAttempt CTime POSIXTime where
    convertAttempt = return . realToFrac
instance ConvertAttempt POSIXTime CTime where
    convertAttempt = return . fromInteger . truncate

instance ConvertAttempt CTime Integer where
    convertAttempt = return . truncate . toRational
instance ConvertAttempt Integer CTime where
    convertAttempt = return . fromInteger

instance ConvertAttempt CTime Double where
    convertAttempt = return . realToFrac
instance ConvertAttempt Double CTime where
    convertAttempt = return . fromInteger . truncate

instance ConvertAttempt CTime Int where
    convertAttempt x = do r1 <- convertAttempt x
                          boundedConversion (return . fromInteger) r1
instance ConvertAttempt Int CTime where
    convertAttempt = convertAttempt . toInteger
instance ConvertSuccess Int CTime where
    convertSuccess = convertSuccess . toInteger

instance ConvertSuccess CTime UTCTime where
    convertSuccess = convertSuccessVia (undefined :: POSIXTime)
instance ConvertSuccess UTCTime CTime where
    convertSuccess = convertSuccessVia (undefined :: POSIXTime)
instance ConvertAttempt CTime UTCTime where
    convertAttempt = convertAttemptVia (undefined :: POSIXTime)
instance ConvertAttempt UTCTime CTime where
    convertAttempt = convertAttemptVia (undefined :: POSIXTime)

instance ConvertSuccess CTime ST.ClockTime where
    convertSuccess = convertSuccessVia (undefined :: POSIXTime)
instance ConvertSuccess ST.ClockTime CTime where
    convertSuccess = convertSuccessVia (undefined :: POSIXTime)
instance ConvertAttempt CTime ST.ClockTime where
    convertAttempt = convertAttemptVia (undefined :: POSIXTime)
instance ConvertAttempt ST.ClockTime CTime where
    convertAttempt = convertAttemptVia (undefined :: POSIXTime)

instance ConvertSuccess CTime ST.CalendarTime where
    convertSuccess = convertSuccessVia (undefined::POSIXTime)
instance ConvertSuccess ST.CalendarTime CTime where
    convertSuccess = convertSuccessVia (undefined::POSIXTime)
instance ConvertAttempt CTime ST.CalendarTime where
    convertAttempt = convertAttemptVia (undefined::POSIXTime)
instance ConvertAttempt ST.CalendarTime CTime where
    convertAttempt = convertAttemptVia (undefined::POSIXTime)

instance ConvertSuccess CTime ZonedTime where
    convertSuccess = convertSuccessVia (undefined::POSIXTime)
instance ConvertSuccess ZonedTime CTime where
    convertSuccess = convertSuccessVia (undefined::POSIXTime)
instance ConvertAttempt CTime ZonedTime where
    convertAttempt = convertAttemptVia (undefined::POSIXTime)
instance ConvertAttempt ZonedTime CTime where
    convertAttempt = convertAttemptVia (undefined::POSIXTime)
