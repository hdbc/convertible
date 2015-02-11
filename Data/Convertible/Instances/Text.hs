{-# LANGUAGE CPP #-}

{- |
   Module     : Data.Convertible.Instances.Text
   Copyright  : Copyright (C) 2011 MailRank, Inc.
   License    : LGPL

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Text instances for Convertible.

Copyright (C) 2011 MailRank, Inc. <bos@mailrank.com>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

These instances perform conversion between text-like types such as
Text, ByteString, and the like.

The instances do /not/ include conversions between ByteString and
Text or String, since such conversions cannot safely be performed
without knowing the encoding of the ByteString.
-}

module Data.Convertible.Instances.Text()
where

import Data.Convertible.Base
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Word (Word8)
import Data.Foldable



instance Convertible TS.Text [Char] where
    {-# INLINE safeConvert #-}
    safeConvert = Right . TS.unpack

instance Convertible TS.Text TL.Text where
    {-# INLINE safeConvert #-}
    safeConvert = Right . TL.fromStrict

instance Convertible TS.Text TLB.Builder where
    {-# INLINE safeConvert #-}
    safeConvert = Right . TLB.fromText

instance Convertible TS.Text BS.ByteString where
    {-# INLINE safeConvert #-}
    safeConvert = Right . TE.encodeUtf8

instance Convertible TS.Text BL.ByteString where
    {-# INLINE safeConvert #-}
    safeConvert = Right . BL.fromStrict . TE.encodeUtf8

instance Convertible TS.Text BB.Builder where
    {-# INLINE safeConvert #-}
#if MIN_VERSION_text(1,2,0)
    safeConvert = Right . TE.encodeUtf8Builder
#else
    safeConvert = safeConvert . TE.encodeUtf8
#endif



instance Convertible TL.Text [Char] where
    {-# INLINE safeConvert #-}
    safeConvert = Right . TL.unpack

instance Convertible TL.Text TS.Text where
    {-# INLINE safeConvert #-}
    safeConvert = Right . TL.toStrict

instance Convertible TL.Text TLB.Builder where
    {-# INLINE safeConvert #-}
    safeConvert = Right . TLB.fromLazyText

instance Convertible TL.Text BS.ByteString where
    {-# INLINE safeConvert #-}
    safeConvert = safeConvert . TLE.encodeUtf8

instance Convertible TL.Text BL.ByteString where
    {-# INLINE safeConvert #-}
    safeConvert = Right . TLE.encodeUtf8

instance Convertible TL.Text BB.Builder where
    {-# INLINE safeConvert #-}
#if MIN_VERSION_text(1,2,0)
    safeConvert = Right . TLE.encodeUtf8Builder
#else
    safeConvert = safeConvert . TLE.encodeUtf8
#endif



instance Convertible TLB.Builder [Char] where
    {-# INLINE safeConvert #-}
    safeConvert = safeConvert . TLB.toLazyText

instance Convertible TLB.Builder TS.Text where
    {-# INLINE safeConvert #-}
    safeConvert = safeConvert . TLB.toLazyText

instance Convertible TLB.Builder TL.Text where
    {-# INLINE safeConvert #-}
    safeConvert = Right . TLB.toLazyText

instance Convertible TLB.Builder BS.ByteString where
    {-# INLINE safeConvert #-}
    safeConvert = safeConvert . TLB.toLazyText

instance Convertible TLB.Builder BL.ByteString where
    {-# INLINE safeConvert #-}
    safeConvert = safeConvert . TLB.toLazyText

instance Convertible TLB.Builder BB.Builder where
    {-# INLINE safeConvert #-}
    safeConvert = safeConvert . TLB.toLazyText



instance Convertible BS.ByteString [Word8] where
    {-# INLINE safeConvert #-}
    safeConvert = Right . BS.unpack

instance Convertible BS.ByteString TS.Text where
    {-# INLINE safeConvert #-}
    safeConvert = Right . TE.decodeUtf8

instance Convertible BS.ByteString TL.Text where
    {-# INLINE safeConvert #-}
    safeConvert = fmap TL.fromStrict . safeConvert

instance Convertible BS.ByteString TLB.Builder where
    {-# INLINE safeConvert #-}
    safeConvert = fmap TLB.fromText . safeConvert

instance Convertible BS.ByteString BL.ByteString where
    {-# INLINE safeConvert #-}
    safeConvert = Right . BL.fromStrict

instance Convertible BS.ByteString BB.Builder where
    {-# INLINE safeConvert #-}
    safeConvert = Right . BB.byteString



instance Convertible BL.ByteString [Word8] where
    {-# INLINE safeConvert #-}
    safeConvert = Right . BL.unpack

instance Convertible BL.ByteString TS.Text where
    {-# INLINE safeConvert #-}
    safeConvert = fmap TL.toStrict . safeConvert

instance Convertible BL.ByteString TL.Text where
    {-# INLINE safeConvert #-}
    safeConvert = Right . TLE.decodeUtf8

instance Convertible BL.ByteString TLB.Builder where
    {-# INLINE safeConvert #-}
    safeConvert = fmap TLB.fromLazyText . safeConvert

instance Convertible BL.ByteString BS.ByteString where
    {-# INLINE safeConvert #-}
    safeConvert = Right . BL.toStrict

instance Convertible BL.ByteString BB.Builder where
    {-# INLINE safeConvert #-}
    safeConvert = Right . BB.lazyByteString



instance Convertible [Char] TS.Text where
    {-# INLINE safeConvert #-}
    safeConvert = Right . TS.pack

instance Convertible [Char] TL.Text where
    {-# INLINE safeConvert #-}
    safeConvert = Right . TL.pack

instance Convertible [Char] TLB.Builder where
    {-# INLINE safeConvert #-}
    safeConvert = Right . TLB.fromString



instance Convertible [Word8] BS.ByteString where
    {-# INLINE safeConvert #-}
    safeConvert = Right . BS.pack

instance Convertible [Word8] BL.ByteString where
    {-# INLINE safeConvert #-}
    safeConvert = Right . BL.pack

instance Convertible [Word8] BB.Builder where
    {-# INLINE safeConvert #-}
    safeConvert = Right . foldMap BB.word8

