{- |
   Module     : Data.Convertible.Instances.C
   Copyright  : Copyright (C) 2009 John Goerzen
   License    : LGPL

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Numeric instances for Convertible for C types.  See comments in
"Data.Convertible.Instances.Num".

Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT
-}
module Data.Convertible.Instances.C()
where

import Data.Convertible.Base
import Data.Convertible.Utils
import Data.Convertible.Instances.Num()
import Data.Int
import Data.Word
import Foreign.C.Types

-- Section 1
instance Convertible CFloat Int where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Int CFloat where 
    safeConvert = return . fromIntegral

instance Convertible CFloat Int8 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Int8 CFloat where 
    safeConvert = return . fromIntegral

instance Convertible CFloat Int16 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Int16 CFloat where 
    safeConvert = return . fromIntegral

instance Convertible CFloat Int32 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Int32 CFloat where 
    safeConvert = return . fromIntegral

instance Convertible CFloat Int64 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Int64 CFloat where 
    safeConvert = return . fromIntegral

instance Convertible CFloat Word where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Word CFloat where 
    safeConvert = return . fromIntegral

instance Convertible CFloat Word8 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Word8 CFloat where 
    safeConvert = return . fromIntegral

instance Convertible CFloat Word16 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Word16 CFloat where 
    safeConvert = return . fromIntegral

instance Convertible CFloat Word32 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Word32 CFloat where 
    safeConvert = return . fromIntegral

instance Convertible CFloat Word64 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Word64 CFloat where 
    safeConvert = return . fromIntegral

instance Convertible CDouble Int where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Int CDouble where 
    safeConvert = return . fromIntegral

instance Convertible CDouble Int8 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Int8 CDouble where 
    safeConvert = return . fromIntegral

instance Convertible CDouble Int16 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Int16 CDouble where 
    safeConvert = return . fromIntegral

instance Convertible CDouble Int32 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Int32 CDouble where 
    safeConvert = return . fromIntegral

instance Convertible CDouble Int64 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Int64 CDouble where 
    safeConvert = return . fromIntegral

instance Convertible CDouble Word where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Word CDouble where 
    safeConvert = return . fromIntegral

instance Convertible CDouble Word8 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Word8 CDouble where 
    safeConvert = return . fromIntegral

instance Convertible CDouble Word16 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Word16 CDouble where 
    safeConvert = return . fromIntegral

instance Convertible CDouble Word32 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Word32 CDouble where 
    safeConvert = return . fromIntegral

instance Convertible CDouble Word64 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Word64 CDouble where 
    safeConvert = return . fromIntegral

instance Convertible CLDouble Int where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Int CLDouble where 
    safeConvert = return . fromIntegral

instance Convertible CLDouble Int8 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Int8 CLDouble where 
    safeConvert = return . fromIntegral

instance Convertible CLDouble Int16 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Int16 CLDouble where 
    safeConvert = return . fromIntegral

instance Convertible CLDouble Int32 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Int32 CLDouble where 
    safeConvert = return . fromIntegral

instance Convertible CLDouble Int64 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Int64 CLDouble where 
    safeConvert = return . fromIntegral

instance Convertible CLDouble Word where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Word CLDouble where 
    safeConvert = return . fromIntegral

instance Convertible CLDouble Word8 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Word8 CLDouble where 
    safeConvert = return . fromIntegral

instance Convertible CLDouble Word16 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Word16 CLDouble where 
    safeConvert = return . fromIntegral

instance Convertible CLDouble Word32 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Word32 CLDouble where 
    safeConvert = return . fromIntegral

instance Convertible CLDouble Word64 where 
    safeConvert = boundedConversion (return . truncate)
instance Convertible Word64 CLDouble where 
    safeConvert = return . fromIntegral

-- Section 2
instance Convertible CFloat Double where
    safeConvert = return . realToFrac
instance Convertible Double CFloat where
    safeConvert = return . realToFrac

instance Convertible CFloat Float where
    safeConvert = return . realToFrac
instance Convertible Float CFloat where
    safeConvert = return . realToFrac

instance Convertible CFloat Rational where
    safeConvert = return . realToFrac
instance Convertible Rational CFloat where
    safeConvert = return . realToFrac

instance Convertible CDouble Double where
    safeConvert = return . realToFrac
instance Convertible Double CDouble where
    safeConvert = return . realToFrac

instance Convertible CDouble Float where
    safeConvert = return . realToFrac
instance Convertible Float CDouble where
    safeConvert = return . realToFrac

instance Convertible CDouble Rational where
    safeConvert = return . realToFrac
instance Convertible Rational CDouble where
    safeConvert = return . realToFrac

instance Convertible CLDouble Double where
    safeConvert = return . realToFrac
instance Convertible Double CLDouble where
    safeConvert = return . realToFrac

instance Convertible CLDouble Float where
    safeConvert = return . realToFrac
instance Convertible Float CLDouble where
    safeConvert = return . realToFrac

instance Convertible CLDouble Rational where
    safeConvert = return . realToFrac
instance Convertible Rational CLDouble where
    safeConvert = return . realToFrac

-- Section 3
instance Convertible CChar Int where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar Int8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int8 CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar Int16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int16 CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar Int32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int32 CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar Int64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int64 CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar Word where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar Word8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word8 CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar Word16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word16 CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar Word32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word32 CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar Word64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word64 CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar Int where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar Int8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int8 CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar Int16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int16 CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar Int32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int32 CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar Int64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int64 CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar Word where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar Word8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word8 CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar Word16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word16 CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar Word32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word32 CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar Word64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word64 CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar Int where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar Int8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int8 CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar Int16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int16 CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar Int32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int32 CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar Int64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int64 CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar Word where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar Word8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word8 CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar Word16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word16 CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar Word32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word32 CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar Word64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word64 CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort Int where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort Int8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int8 CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort Int16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int16 CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort Int32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int32 CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort Int64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int64 CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort Word where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort Word8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word8 CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort Word16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word16 CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort Word32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word32 CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort Word64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word64 CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort Int where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort Int8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int8 CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort Int16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int16 CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort Int32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int32 CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort Int64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int64 CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort Word where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort Word8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word8 CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort Word16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word16 CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort Word32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word32 CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort Word64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word64 CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt Int where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt Int8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int8 CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt Int16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int16 CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt Int32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int32 CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt Int64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int64 CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt Word where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt Word8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word8 CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt Word16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word16 CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt Word32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word32 CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt Word64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word64 CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt Int where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt Int8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int8 CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt Int16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int16 CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt Int32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int32 CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt Int64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int64 CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt Word where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt Word8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word8 CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt Word16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word16 CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt Word32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word32 CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt Word64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word64 CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong Int where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong Int8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int8 CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong Int16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int16 CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong Int32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int32 CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong Int64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int64 CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong Word where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong Word8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word8 CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong Word16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word16 CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong Word32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word32 CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong Word64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word64 CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong Int where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong Int8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int8 CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong Int16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int16 CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong Int32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int32 CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong Int64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int64 CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong Word where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong Word8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word8 CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong Word16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word16 CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong Word32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word32 CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong Word64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word64 CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize Int where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize Int8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int8 CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize Int16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int16 CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize Int32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int32 CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize Int64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int64 CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize Word where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize Word8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word8 CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize Word16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word16 CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize Word32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word32 CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize Word64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word64 CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar Int where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar Int8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int8 CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar Int16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int16 CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar Int32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int32 CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar Int64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int64 CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar Word where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar Word8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word8 CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar Word16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word16 CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar Word32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word32 CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar Word64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word64 CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong Int where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong Int8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int8 CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong Int16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int16 CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong Int32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int32 CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong Int64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int64 CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong Word where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong Word8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word8 CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong Word16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word16 CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong Word32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word32 CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong Word64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word64 CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong Int where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong Int8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int8 CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong Int16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int16 CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong Int32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int32 CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong Int64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Int64 CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong Word where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong Word8 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word8 CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong Word16 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word16 CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong Word32 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word32 CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong Word64 where
    safeConvert = boundedConversion (return . fromIntegral)
instance Convertible Word64 CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

-- Section 4
instance Convertible CChar CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CChar CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

-- Section 5
instance Convertible CFloat CDouble where
    safeConvert = return . realToFrac

instance Convertible CFloat CLDouble where
    safeConvert = return . realToFrac

instance Convertible CDouble CFloat where
    safeConvert = return . realToFrac

instance Convertible CDouble CLDouble where
    safeConvert = return . realToFrac

instance Convertible CLDouble CFloat where
    safeConvert = return . realToFrac

instance Convertible CLDouble CDouble where
    safeConvert = return . realToFrac

-- Section 6
instance Convertible CFloat Integer where
    safeConvert = return . truncate
instance Convertible Integer CFloat where
    safeConvert = return . fromIntegral

instance Convertible CDouble Integer where
    safeConvert = return . truncate
instance Convertible Integer CDouble where
    safeConvert = return . fromIntegral

instance Convertible CLDouble Integer where
    safeConvert = return . truncate
instance Convertible Integer CLDouble where
    safeConvert = return . fromIntegral

-- Section 7
instance Convertible CChar Integer where
    safeConvert = return . fromIntegral
instance Convertible Integer CChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSChar Integer where
    safeConvert = return . fromIntegral
instance Convertible Integer CSChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUChar Integer where
    safeConvert = return . fromIntegral
instance Convertible Integer CUChar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CShort Integer where
    safeConvert = return . fromIntegral
instance Convertible Integer CShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUShort Integer where
    safeConvert = return . fromIntegral
instance Convertible Integer CUShort where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CInt Integer where
    safeConvert = return . fromIntegral
instance Convertible Integer CInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CUInt Integer where
    safeConvert = return . fromIntegral
instance Convertible Integer CUInt where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLong Integer where
    safeConvert = return . fromIntegral
instance Convertible Integer CLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULong Integer where
    safeConvert = return . fromIntegral
instance Convertible Integer CULong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CSize Integer where
    safeConvert = return . fromIntegral
instance Convertible Integer CSize where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CWchar Integer where
    safeConvert = return . fromIntegral
instance Convertible Integer CWchar where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CLLong Integer where
    safeConvert = return . fromIntegral
instance Convertible Integer CLLong where
    safeConvert = boundedConversion (return . fromIntegral)

instance Convertible CULLong Integer where
    safeConvert = return . fromIntegral
instance Convertible Integer CULLong where
    safeConvert = boundedConversion (return . fromIntegral)

-- Section 8o
instance Convertible CChar Char where
    safeConvert = boundedConversion (return . toEnum . fromIntegral)
instance Convertible Char CChar where
    safeConvert = boundedConversion (return . fromIntegral . fromEnum)

instance Convertible CSChar Char where
    safeConvert = boundedConversion (return . toEnum . fromIntegral)
instance Convertible Char CSChar where
    safeConvert = boundedConversion (return . fromIntegral . fromEnum)

instance Convertible CUChar Char where
    safeConvert = boundedConversion (return . toEnum . fromIntegral)
instance Convertible Char CUChar where
    safeConvert = boundedConversion (return . fromIntegral . fromEnum)

instance Convertible CShort Char where
    safeConvert = boundedConversion (return . toEnum . fromIntegral)
instance Convertible Char CShort where
    safeConvert = boundedConversion (return . fromIntegral . fromEnum)

instance Convertible CUShort Char where
    safeConvert = boundedConversion (return . toEnum . fromIntegral)
instance Convertible Char CUShort where
    safeConvert = boundedConversion (return . fromIntegral . fromEnum)

instance Convertible CInt Char where
    safeConvert = boundedConversion (return . toEnum . fromIntegral)
instance Convertible Char CInt where
    safeConvert = boundedConversion (return . fromIntegral . fromEnum)

instance Convertible CUInt Char where
    safeConvert = boundedConversion (return . toEnum . fromIntegral)
instance Convertible Char CUInt where
    safeConvert = boundedConversion (return . fromIntegral . fromEnum)

instance Convertible CLong Char where
    safeConvert = boundedConversion (return . toEnum . fromIntegral)
instance Convertible Char CLong where
    safeConvert = boundedConversion (return . fromIntegral . fromEnum)

instance Convertible CULong Char where
    safeConvert = boundedConversion (return . toEnum . fromIntegral)
instance Convertible Char CULong where
    safeConvert = boundedConversion (return . fromIntegral . fromEnum)

instance Convertible CSize Char where
    safeConvert = boundedConversion (return . toEnum . fromIntegral)
instance Convertible Char CSize where
    safeConvert = boundedConversion (return . fromIntegral . fromEnum)

instance Convertible CWchar Char where
    safeConvert = boundedConversion (return . toEnum . fromIntegral)
instance Convertible Char CWchar where
    safeConvert = boundedConversion (return . fromIntegral . fromEnum)

instance Convertible CLLong Char where
    safeConvert = boundedConversion (return . toEnum . fromIntegral)
instance Convertible Char CLLong where
    safeConvert = boundedConversion (return . fromIntegral . fromEnum)

instance Convertible CULLong Char where
    safeConvert = boundedConversion (return . toEnum . fromIntegral)
instance Convertible Char CULLong where
    safeConvert = boundedConversion (return . fromIntegral . fromEnum)

