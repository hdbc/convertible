{-# LANGUAGE MultiParamTypeClasses #-}
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

module Data.Convertible.Utils(
                             )
where
import Data.Convertible.Base

{- | Utility function to perform bounds checking as part of a conversion.

Does this be examining the bounds of the destination type, converting to the type of
the source via 'safeConvert', comparing to the source value.  Results in an error
if the conversion is out of bounds. -}
boundedConversion :: (Ord a, Bounded a, Bounded b, Show a, Show b, Convertible b a,
                      ConvTypeName a, ConvTypeName b) => 
                     a                      -- ^ Input data
                  -> (a -> ConvertResult b) -- ^ Function to do the conversion
                  -> ConvertResult b        -- ^ Result
boundedConversion inp func =
    do result <- func inp
       let smallest = asTypeOf minBound result
       let biggest = asTypeOf maxBound result
       let smallest' = convert smallest
       let biggest' = convert biggest
       if inp < smallest' || inp > biggest'
          then convError ("Input value outside of bounds: " ++ show (smallest, biggest))
               inp
          else return result
