{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.Convertible.Instances.Num
   Copyright  : Copyright (C) 2009 John Goerzen
   License    : LGPL

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

-}

module Data.Convertible.Instances.Num()
where

import Data.Convertible.Base
import Data.Convertible.Utils
import Data.Int
import Data.Word

{- | Conversion between Double and things like Int, Int32, Word32, etc. -}
instance (Bounded b, Integral b, Show b, Convertible b Double, ConvTypeName b) => 
    Convertible Double b where
    safeConvert = boundedConversion (return . truncate)

{- | Conversion between Float and bounded integers. -}
instance (Bounded b, Integral b, Show b, Convertible b Float, ConvTypeName b) => 
    Convertible Float b where
    safeConvert = boundedConversion (return . truncate)

instance Convertible Double Integer where
    safeConvert = return . truncate
instance Convertible Float Integer where
    safeConvert = return . truncate

instance (Bounded a, Integral a) => Convertible a Double where
    safeConvert = return . fromIntegral
instance (Bounded a, Integral a) => Convertible a Float where
    safeConvert = return . fromIntegral


{-
instance Convertible Double Int where
    safeConvert = boundedConversion (return . truncate)
instance Convertible Float Int where
    safeConvert = boundedConversion (return . truncate)
-}
{-
instance Convertible Int Double where
    safeConvert = return . fromIntegral
instance Convertible Int Float where
    safeConvert = return . fromIntegral
-}
{-
instance Convertible Double Integer where
    safeConvert = return . truncate
instance Convertible Integer Double where
    safeConvert = return . fromIntegral
-}