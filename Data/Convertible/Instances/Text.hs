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
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)

-- Text

instance Convertible [Char] TS.Text where
    safeConvert = Right . TS.pack

instance Convertible TS.Text [Char] where
    safeConvert = Right . TS.unpack

instance Convertible [Char] TL.Text where
    safeConvert = Right . TL.pack

instance Convertible TL.Text [Char] where
    safeConvert = Right . TL.unpack

instance Convertible TS.Text TL.Text where
    safeConvert = Right . TL.fromStrict

instance Convertible TL.Text TS.Text where
    safeConvert = Right . TL.toStrict

-- ByteString

instance Convertible [Word8] BS.ByteString where
    safeConvert = Right . BS.pack

instance Convertible BS.ByteString [Word8] where
    safeConvert = Right . BS.unpack

instance Convertible [Word8] BL.ByteString where
    safeConvert = Right . BL.pack

instance Convertible BL.ByteString [Word8] where
    safeConvert = Right . BL.unpack

instance Convertible BS.ByteString BL.ByteString where
    safeConvert = Right . BL.fromChunks . (:[])

instance Convertible BL.ByteString BS.ByteString where
    safeConvert = Right . BS.concat . BL.toChunks
