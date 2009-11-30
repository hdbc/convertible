{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
---------------------------------------------------------
--
-- Module        : Data.Convertible.Instances.String
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
---------------------------------------------------------

-- | Instances of 'ConvertSuccess' and 'ConvertAttempt' for 'String', along
-- with instances for bytestrings and text (lazy and strict).
module Data.Convertible.Instances.String
    ( InvalidDayException (..)
    , InvalidBoolException (..)
    ) where

import Data.Convertible.Base
import Data.Typeable (Typeable)
import Control.Exception (Exception)
import qualified Safe.Failure as SF
import Data.Convertible.Instances.Text ()
import Data.Attempt
import Control.Monad ((<=<))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT

import Data.Time.Calendar
import Data.Ratio (Ratio)

{- Not needed yet
fromString :: ConvertSuccess String a => String -> a
fromString = convertSuccess
-}

fromStringA :: ConvertAttempt String a => String -> Attempt a
fromStringA = convertAttempt

toString :: ConvertSuccess a String => a -> String
toString = convertSuccess

-- Day
data InvalidDayException = InvalidDayException String
    deriving (Show, Typeable)
instance Exception InvalidDayException

instance ConvertSuccess Day [Char] where
    convertSuccess = show
instance ConvertAttempt Day [Char] where
    convertAttempt = return . convertSuccess
instance ConvertAttempt [Char] Day where
    convertAttempt s = wrapFailure (const $ InvalidDayException s) $ do
        SF.assert (length s == 10) () $ InvalidDayException s
        y <- SF.read $ take 4 s
        m <- SF.read $ take 2 $ drop 5 s
        d <- SF.read $ take 2 $ drop 8 s
        return $ fromGregorian y m d

-- Bool
data InvalidBoolException = InvalidBoolException String
    deriving (Show, Typeable)
instance Exception InvalidBoolException

instance ConvertAttempt Bool [Char] where
    convertAttempt = return . convertSuccess
instance ConvertSuccess Bool [Char] where
    convertSuccess b = if b then "true" else "false"
instance ConvertAttempt [Char] Bool where
    convertAttempt s =
        case s of
            -- list comes from http://yaml.org/type/bool.html
            "y" -> return True
            "Y" -> return True
            "yes" -> return True
            "Yes" -> return True
            "YES" -> return True
            "true" -> return True
            "True" -> return True
            "TRUE" -> return True
            "on" -> return True
            "On" -> return True
            "ON" -> return True

            "n" -> return False
            "N" -> return False
            "no" -> return False
            "No" -> return False
            "NO" -> return False
            "false" -> return False
            "False" -> return False
            "FALSE" -> return False
            "off" -> return False
            "Off" -> return False
            "OFF" -> return False

            _ -> failure $ InvalidBoolException s

-- Int
instance ConvertSuccess Int [Char] where
    convertSuccess = show
instance ConvertAttempt Int [Char] where
    convertAttempt = return . convertSuccess
instance ConvertAttempt [Char] Int where
    convertAttempt = SF.read

-- Rational
instance ConvertSuccess (Ratio Integer) [Char] where
    convertSuccess = show
instance ConvertAttempt (Ratio Integer) [Char] where
    convertAttempt = return . convertSuccess
instance ConvertAttempt [Char] (Ratio Integer) where
    convertAttempt = SF.read

-- Instances for bytestrings and text
instance ConvertAttempt BS.ByteString Day where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt BL.ByteString Day where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt ST.Text Day where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt LT.Text Day where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt BS.ByteString Bool where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt BL.ByteString Bool where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt ST.Text Bool where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt LT.Text Bool where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt BS.ByteString Int where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt BL.ByteString Int where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt ST.Text Int where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt LT.Text Int where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt BS.ByteString (Ratio Integer) where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt BL.ByteString (Ratio Integer) where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt ST.Text (Ratio Integer) where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt LT.Text (Ratio Integer) where
    convertAttempt = fromStringA <=< convertAttempt
instance ConvertAttempt Day BS.ByteString where
    convertAttempt = convertAttempt . toString
instance ConvertAttempt Day BL.ByteString where
    convertAttempt = convertAttempt . toString
instance ConvertAttempt Day ST.Text where
    convertAttempt = convertAttempt . toString
instance ConvertAttempt Day LT.Text where
    convertAttempt = convertAttempt . toString
instance ConvertAttempt Bool BS.ByteString where
    convertAttempt = convertAttempt . toString
instance ConvertAttempt Bool BL.ByteString where
    convertAttempt = convertAttempt . toString
instance ConvertAttempt Bool ST.Text where
    convertAttempt = convertAttempt . toString
instance ConvertAttempt Bool LT.Text where
    convertAttempt = convertAttempt . toString
instance ConvertAttempt Int BS.ByteString where
    convertAttempt = convertAttempt . toString
instance ConvertAttempt Int BL.ByteString where
    convertAttempt = convertAttempt . toString
instance ConvertAttempt Int ST.Text where
    convertAttempt = convertAttempt . toString
instance ConvertAttempt Int LT.Text where
    convertAttempt = convertAttempt . toString
instance ConvertAttempt (Ratio Integer) BS.ByteString where
    convertAttempt = convertAttempt . toString
instance ConvertAttempt (Ratio Integer) BL.ByteString where
    convertAttempt = convertAttempt . toString
instance ConvertAttempt (Ratio Integer) ST.Text where
    convertAttempt = convertAttempt . toString
instance ConvertAttempt (Ratio Integer) LT.Text where
    convertAttempt = convertAttempt . toString
instance ConvertSuccess Day BS.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Day BL.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Day ST.Text where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Day LT.Text where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Bool BS.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Bool BL.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Bool ST.Text where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Bool LT.Text where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Int BS.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Int BL.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Int ST.Text where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess Int LT.Text where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess (Ratio Integer) BS.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess (Ratio Integer) BL.ByteString where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess (Ratio Integer) ST.Text where
    convertSuccess = convertSuccess . toString
instance ConvertSuccess (Ratio Integer) LT.Text where
    convertSuccess = convertSuccess . toString
