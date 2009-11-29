{- |
   Module     : Data.Convertible.Instances.Time
   Copyright  : Copyright (C) 2009 John Goerzen
   License    : LGPL

   Maintainer : John Goerzen <jgoerzen@complete.org>
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
#ifndef TIME_GT_113
import Data.Typeable
#endif
import Data.Ratio
import Foreign.C.Types

----------------------------------------------------------------------
-- Intra-System.Time stuff
----------------------------------------------------------------------

instance Convertible ST.ClockTime ST.CalendarTime where
    safeConvert = return . ST.toUTCTime

instance Convertible ST.CalendarTime ST.ClockTime where
    safeConvert = return . ST.toClockTime

instance Convertible ST.ClockTime Integer where
    safeConvert (ST.TOD x _) = return x

instance Convertible Integer ST.ClockTime where
    safeConvert x = return $ ST.TOD x 0

----------------------------------------------------------------------
-- Intra-Data.Time stuff
----------------------------------------------------------------------

------------------------------ POSIX and UTC times

#ifndef TIME_GT_113
instance Typeable NominalDiffTime where
    typeOf _ = mkTypeName "NominalDiffTime"

instance Typeable UTCTime where
    typeOf _ = mkTypeName "UTCTime"
#endif

{- Covered under Real a
instance Convertible Rational POSIXTime where
    safeConvert = return . fromRational
-}

instance Convertible Rational POSIXTime where
    safeConvert = return . fromRational
instance Convertible Integer POSIXTime where
    safeConvert = return . fromInteger
instance Convertible Int POSIXTime where
    safeConvert = return . fromIntegral
instance Convertible Double POSIXTime where
    safeConvert = return . realToFrac

instance Convertible POSIXTime Integer where
    safeConvert = return . truncate
instance Convertible POSIXTime Rational where
    safeConvert = return . toRational
instance Convertible POSIXTime Double where
    safeConvert = return . realToFrac
instance Convertible POSIXTime Int where
    safeConvert = boundedConversion (return . truncate)

instance Convertible POSIXTime UTCTime where
    safeConvert = return . posixSecondsToUTCTime
instance Convertible UTCTime POSIXTime where
    safeConvert = return . utcTimeToPOSIXSeconds

instance Convertible Rational UTCTime where
    safeConvert a = safeConvert a >>= return . posixSecondsToUTCTime
instance Convertible Integer UTCTime where
    safeConvert a = safeConvert a >>= return . posixSecondsToUTCTime
instance Convertible Int UTCTime where
    safeConvert a = safeConvert a >>= return . posixSecondsToUTCTime
instance Convertible Double UTCTime where
    safeConvert a = safeConvert a >>= return . posixSecondsToUTCTime

instance Convertible UTCTime Rational where
    safeConvert = safeConvert . utcTimeToPOSIXSeconds
instance Convertible UTCTime Integer where
    safeConvert = safeConvert . utcTimeToPOSIXSeconds
instance Convertible UTCTime Double where
    safeConvert = safeConvert . utcTimeToPOSIXSeconds
instance Convertible UTCTime Int where
    safeConvert = boundedConversion (safeConvert . utcTimeToPOSIXSeconds)

------------------------------ LocalTime stuff

instance Convertible UTCTime ZonedTime where
    safeConvert = return . utcToZonedTime utc
instance Convertible POSIXTime ZonedTime where
    safeConvert = return . utcToZonedTime utc . posixSecondsToUTCTime
instance Convertible ZonedTime UTCTime where
    safeConvert = return . zonedTimeToUTC
instance Convertible ZonedTime POSIXTime where
    safeConvert = return . utcTimeToPOSIXSeconds . zonedTimeToUTC

{- Too obvious?
instance Convertible LocalTime Day where
    safeConvert = return . localDay
instance Convertible LocalTime TimeOfDay where
    safeConvert = return . localTimeOfDay
-}

----------------------------------------------------------------------
-- Conversions between old and new time
----------------------------------------------------------------------
instance Convertible ST.CalendarTime ZonedTime where
    safeConvert ct = return $ ZonedTime {
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

instance Convertible ST.CalendarTime POSIXTime where
    safeConvert = convertVia (undefined::ST.ClockTime)
instance Convertible ST.CalendarTime UTCTime where
    safeConvert = convertVia (undefined::POSIXTime)

instance Convertible ST.ClockTime POSIXTime where
    safeConvert (ST.TOD x y) = return $ fromRational $ 
                                        fromInteger x + fromRational (y % 1000000000000)
instance Convertible ST.ClockTime UTCTime where
    safeConvert = convertVia (undefined::POSIXTime)
instance Convertible ST.ClockTime ZonedTime where
    safeConvert = convertVia (undefined::UTCTime)
instance Convertible ZonedTime ST.ClockTime where
    safeConvert = convertVia (undefined::POSIXTime)

instance Convertible POSIXTime ST.ClockTime where
    safeConvert x = return $ ST.TOD rsecs rpico
        where rsecs = floor x
              rpico = truncate $ abs $ 1000000000000 * (x - (fromIntegral rsecs))
instance Convertible UTCTime ST.ClockTime where
    safeConvert = safeConvert . utcTimeToPOSIXSeconds

instance Convertible ZonedTime ST.CalendarTime where
    safeConvert zt = return $ ST.CalendarTime {
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
instance Convertible POSIXTime ST.CalendarTime where
    safeConvert = convertVia (undefined::ZonedTime)
instance Convertible UTCTime ST.CalendarTime where
    safeConvert = safeConvert . utcTimeToPOSIXSeconds

instance Convertible ST.TimeDiff NominalDiffTime where
    {- This is a clever hack.  We convert the TimeDiff to a ClockTime, applying
       it as a diff vs. the epoch.  Converting this ClockTime to a POSIXTime yiels
       the NominalDiffTime we want, since a POSIXTime is a NominalDiffTime vs. the
       epoch. -}
    safeConvert td = safeConvert clockTime
        where clockTime = ST.addToClockTime td (ST.TOD 0 0)
instance Convertible NominalDiffTime ST.TimeDiff where
    {- Similar clever hack as above. -}
    safeConvert ndt =
        do clockt <- safeConvert ndt
           return (ST.diffClockTimes clockt (ST.TOD 0 0))

instance Convertible Integer ST.TimeDiff where
    safeConvert = convertVia (undefined::NominalDiffTime)
instance Convertible Double ST.TimeDiff where
    safeConvert = convertVia (undefined::NominalDiffTime)
instance Convertible ST.TimeDiff Integer where
    safeConvert = convertVia (undefined :: NominalDiffTime)
instance Convertible ST.TimeDiff Rational where
    safeConvert = convertVia (undefined :: NominalDiffTime)
instance Convertible ST.TimeDiff Double where
    safeConvert = convertVia (undefined :: NominalDiffTime)

----------------------------------------------------------------------
-- Foreign.C Types
----------------------------------------------------------------------

instance Convertible CTime POSIXTime where
    safeConvert = return . realToFrac
instance Convertible POSIXTime CTime where
    safeConvert = return . fromInteger . truncate

instance Convertible CTime Integer where
    safeConvert = return . truncate . toRational
instance Convertible Integer CTime where
    safeConvert = return . fromInteger

instance Convertible CTime Double where
    safeConvert = return . realToFrac
instance Convertible Double CTime where
    safeConvert = return . fromInteger . truncate

instance Convertible CTime Int where
    safeConvert x = do r1 <- safeConvert x
                       boundedConversion (return . fromInteger) r1
instance Convertible Int CTime where
    safeConvert = safeConvert . toInteger

instance Convertible CTime UTCTime where
    safeConvert = convertVia (undefined :: POSIXTime)
instance Convertible UTCTime CTime where
    safeConvert = convertVia (undefined :: POSIXTime)

instance Convertible CTime ST.ClockTime where
    safeConvert = convertVia (undefined :: POSIXTime)
instance Convertible ST.ClockTime CTime where
    safeConvert = convertVia (undefined :: POSIXTime)

instance Convertible CTime ST.CalendarTime where
    safeConvert = convertVia (undefined::POSIXTime)
instance Convertible ST.CalendarTime CTime where
    safeConvert = convertVia (undefined::POSIXTime)

instance Convertible CTime ZonedTime where
    safeConvert = convertVia (undefined::POSIXTime)
instance Convertible ZonedTime CTime where
    safeConvert = convertVia (undefined::POSIXTime)
