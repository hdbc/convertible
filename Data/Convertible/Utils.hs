{-# LANGUAGE DeriveDataTypeable #-}
{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.Convertible.Utils
   Copyright  : Copyright (C) 2009 John Goerzen
   License    : LGPL

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

-}

module Data.Convertible.Utils(boundedConversion,
                             ConvertBoundsException (..),
                             mkTypeName,
                             convertAttemptVia,
                             convertSuccessVia
                             )
where
import Data.Convertible.Base
import Data.Typeable
import Data.Attempt
import Control.Exception (Exception)

{- | Utility function to perform bounds checking as part of a conversion.

Does this be examining the bounds of the destination type, converting to the type of
the source via 'safeConvert', comparing to the source value.  Results in an error
if the conversion is out of bounds. -}
boundedConversion :: (Ord a, Bounded b, Show a, Show b,
                      ConvertAttempt a Integer,
                      ConvertAttempt b Integer,
                      Typeable a, Typeable b) =>
                     (a -> Attempt b) -- ^ Function to do the conversion
                  -> a                -- ^ Input data
                  -> Attempt b        -- ^ Result
boundedConversion func inp =
    do result <- func inp
       let smallest = minBound `asTypeOf` result
           biggest  = maxBound `asTypeOf` result
       smallest' <- convertAttempt smallest :: Attempt Integer
       biggest'  <- convertAttempt biggest  :: Attempt Integer
       inp'      <- convertAttempt inp      :: Attempt Integer
       if inp' < smallest' || inp' > biggest'
          then failure $ ConvertBoundsException smallest biggest inp
          else return result

data ConvertBoundsException v a = ConvertBoundsException v v a
    deriving Typeable
instance (Show v, Show a) => Show (ConvertBoundsException v a) where
    show (ConvertBoundsException x y a) =
        "Input value outside of bounds: " ++ show (x, y) ++ ": " ++ show a
instance (Show v, Show a, Typeable v, Typeable a)
    => Exception (ConvertBoundsException v a)

{- | Useful for defining 'Typeable' instances.  Example:

>instance Typeable TimeOfDay where
>    typeOf _ = mkTypeName "TimeOfDay"
-}
mkTypeName :: String -> TypeRep
mkTypeName name = mkTyConApp (mkTyCon name) []

{- | Useful for defining conversions that are implemented in terms of other
conversions via an intermediary type. Instead of:

>instance Convertible CalendarTime POSIXTime where
>    safeConvert a = do r <- safeConvert a
>                       safeConvert (r :: ClockTime)

we can now write:

>instance Convertible CalendarTime POSIXTime where
>    safeConvert = convertVia (undefined::ClockTime)

which does the same thing -- converts a CalendarTime to a ClockTime, then a
ClockTime to a POSIXTime, both using existing 'Convertible' instances.
 -}
convertAttemptVia :: (ConvertAttempt a b, ConvertAttempt b c) =>
              b                 -- ^ Dummy data to establish intermediate type - can be undefined
           -> a                 -- ^ Input value
           -> Attempt c         -- ^ Result
convertAttemptVia dummy inp =
    do r1 <- convertAttempt inp
       convertAttempt (r1 `asTypeOf` dummy)

{- | Same as 'convertAttemptVia' for 'ConvertSuccess' -}
convertSuccessVia :: (ConvertSuccess a b, ConvertSuccess b c) =>
              b                 -- ^ Dummy data to establish intermediate type - can be undefined
           -> a                 -- ^ Input value
           -> c                 -- ^ Result
convertSuccessVia dummy inp =
    convertSuccess $ convertSuccess inp `asTypeOf` dummy
