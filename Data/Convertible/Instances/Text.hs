{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
---------------------------------------------------------
--
-- Module        : Data.Convertible.Instances.Text
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
---------------------------------------------------------

-- | Instances to convert amongst 'String's, strict bytestrings, lazy
-- bytestrings, strict text and lazy text.
module Data.Convertible.Instances.Text () where

import Data.Convertible.Base
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as STE
import qualified Data.Text.Lazy.Encoding as LTE

toST :: ConvertSuccess a ST.Text => a -> ST.Text
toST = convertSuccess

toLT :: ConvertSuccess a LT.Text => a -> LT.Text
toLT = convertSuccess

instance ConvertSuccess [Char] [Char] where
    convertSuccess = id
instance ConvertSuccess [Char] BS.ByteString where
    convertSuccess = convertSuccess . toST
instance ConvertSuccess [Char] BL.ByteString where
    convertSuccess = convertSuccess . toLT
instance ConvertSuccess [Char] ST.Text where
    convertSuccess = ST.pack
instance ConvertSuccess [Char] LT.Text where
    convertSuccess = LT.pack
instance ConvertSuccess BS.ByteString [Char] where
    convertSuccess = convertSuccess . toST
instance ConvertSuccess BS.ByteString BS.ByteString where
    convertSuccess = id
instance ConvertSuccess BS.ByteString BL.ByteString where
    convertSuccess = BL.fromChunks . return
instance ConvertSuccess BS.ByteString ST.Text where
    convertSuccess = STE.decodeUtf8
instance ConvertSuccess BS.ByteString LT.Text where
    convertSuccess = convertSuccess . toST
instance ConvertSuccess BL.ByteString [Char] where
    convertSuccess = convertSuccess . toLT
instance ConvertSuccess BL.ByteString BS.ByteString where
    convertSuccess = BS.concat . BL.toChunks
instance ConvertSuccess BL.ByteString BL.ByteString where
    convertSuccess = id
instance ConvertSuccess BL.ByteString ST.Text where
    convertSuccess = convertSuccess . BS.concat . BL.toChunks
instance ConvertSuccess BL.ByteString LT.Text where
    convertSuccess = LTE.decodeUtf8
instance ConvertSuccess ST.Text [Char] where
    convertSuccess = ST.unpack
instance ConvertSuccess ST.Text BS.ByteString where
    convertSuccess = STE.encodeUtf8
instance ConvertSuccess ST.Text BL.ByteString where
    convertSuccess = convertSuccess . STE.encodeUtf8
instance ConvertSuccess ST.Text ST.Text where
    convertSuccess = id
instance ConvertSuccess ST.Text LT.Text where
    convertSuccess = LT.fromChunks . return
instance ConvertSuccess LT.Text [Char] where
    convertSuccess = LT.unpack
instance ConvertSuccess LT.Text BS.ByteString where
    convertSuccess = convertSuccess . toST
instance ConvertSuccess LT.Text BL.ByteString where
    convertSuccess = LTE.encodeUtf8
instance ConvertSuccess LT.Text ST.Text where
    convertSuccess = ST.concat . LT.toChunks
instance ConvertSuccess LT.Text LT.Text where
    convertSuccess = id
instance ConvertAttempt [Char] [Char] where
    convertAttempt = return . convertSuccess
instance ConvertAttempt [Char] BS.ByteString where
    convertAttempt = return . convertSuccess
instance ConvertAttempt [Char] BL.ByteString where
    convertAttempt = return . convertSuccess
instance ConvertAttempt [Char] ST.Text where
    convertAttempt = return . convertSuccess
instance ConvertAttempt [Char] LT.Text where
    convertAttempt = return . convertSuccess
instance ConvertAttempt BS.ByteString [Char] where
    convertAttempt = return . convertSuccess
instance ConvertAttempt BS.ByteString BS.ByteString where
    convertAttempt = return . convertSuccess
instance ConvertAttempt BS.ByteString BL.ByteString where
    convertAttempt = return . convertSuccess
instance ConvertAttempt BS.ByteString ST.Text where
    convertAttempt = return . convertSuccess
instance ConvertAttempt BS.ByteString LT.Text where
    convertAttempt = return . convertSuccess
instance ConvertAttempt BL.ByteString [Char] where
    convertAttempt = return . convertSuccess
instance ConvertAttempt BL.ByteString BS.ByteString where
    convertAttempt = return . convertSuccess
instance ConvertAttempt BL.ByteString BL.ByteString where
    convertAttempt = return . convertSuccess
instance ConvertAttempt BL.ByteString ST.Text where
    convertAttempt = return . convertSuccess
instance ConvertAttempt BL.ByteString LT.Text where
    convertAttempt = return . convertSuccess
instance ConvertAttempt ST.Text [Char] where
    convertAttempt = return . convertSuccess
instance ConvertAttempt ST.Text BS.ByteString where
    convertAttempt = return . convertSuccess
instance ConvertAttempt ST.Text BL.ByteString where
    convertAttempt = return . convertSuccess
instance ConvertAttempt ST.Text ST.Text where
    convertAttempt = return . convertSuccess
instance ConvertAttempt ST.Text LT.Text where
    convertAttempt = return . convertSuccess
instance ConvertAttempt LT.Text [Char] where
    convertAttempt = return . convertSuccess
instance ConvertAttempt LT.Text BS.ByteString where
    convertAttempt = return . convertSuccess
instance ConvertAttempt LT.Text BL.ByteString where
    convertAttempt = return . convertSuccess
instance ConvertAttempt LT.Text ST.Text where
    convertAttempt = return . convertSuccess
instance ConvertAttempt LT.Text LT.Text where
    convertAttempt = return . convertSuccess
