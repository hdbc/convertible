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
import qualified System.Time as ST
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar.OrdinalDate

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

{- covered under Real a below
instance Integral a => Convertible a POSIXTime where
    safeConvert = return . fromIntegral
-}

{- Covered under Real a
instance Convertible Rational POSIXTime where
    safeConvert = return . fromRational
-}
instance Real a => Convertible a POSIXTime where
    safeConvert = return . fromRational . toRational

instance Convertible POSIXTime UTCTime where
    safeConvert = return . posixSecondsToUTCTime

--instance Convertible UTCTime POSIXTime where
--    safeConvert = return . utcTimeToPOSIXSeconds

