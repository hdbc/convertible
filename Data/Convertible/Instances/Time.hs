{- |
   Module     : Data.Convertible.Instances.Time
   Copyright  : Copyright (C) 2009 John Goerzen
   License    : LGPL

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Instances to convert between various time structures.

Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

module Data.Convertible.Instances.Time()
where

import Data.Convertible.Base
import Data.Convertible.Utils
import Data.Convertible.Instances.Num
import qualified System.Time as ST
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar.OrdinalDate
import Data.Typeable
import Data.Ratio

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

instance Typeable NominalDiffTime where
    typeOf _ = mkTypeName "NominalDiffTime"

instance Typeable UTCTime where
    typeOf _ = mkTypeName "UTCTime"

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
    safeConvert = return . fromRational . toRational

instance Convertible POSIXTime Integer where
    safeConvert = return . truncate
instance Convertible POSIXTime Rational where
    safeConvert = return . toRational
instance Convertible POSIXTime Double where
    safeConvert = return . fromRational . toRational
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
instance Convertible ZonedTime UTCTime where
    safeConvert = return . zonedTimeToUTC
instance Convertible LocalTime Day where
    safeConvert = return . localDay
instance Convertible LocalTime TimeOfDay where
    safeConvert = return . localTimeOfDay

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


testUTC :: UTCTime
testUTC = convert (51351::Int)
