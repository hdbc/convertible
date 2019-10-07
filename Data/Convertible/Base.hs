{-
Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE

-}

{- |
   Module     : Data.Convertible.Base
   Copyright  : Copyright (C) 2009-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

-}

module Data.Convertible.Base( -- * The conversion process
                              convert,
                              Convertible(..),
                              -- * Handling the results
                              ConvertResult,
                              ConvertError(..),
                              convError,
                              prettyConvertError
                             )
where
import Control.Monad.Fail
import Control.Monad.Error
import Data.Typeable

{- | The result of a safe conversion via 'safeConvert'. -}
type ConvertResult a = Either ConvertError a

instance MonadFail (Either ConvertError) where
    fail = Left . strMsg

----------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------

{- | A typeclass that represents something that can be converted.
A @Convertible a b@ instance represents an @a@ that can be converted to a @b@. -}
class Convertible a b where
    {- | Convert @a@ to @b@, returning Right on success and Left on error.
       For a simpler interface, see 'convert'. -}
    safeConvert :: a -> ConvertResult b

{-
{- | Any type can be converted to itself. -}
instance Convertible a a where
    safeConvert x = return x
-}

{-
{- | Lists of any convertible type can be converted. -}
instance Convertible a b => Convertible [a] [b] where
    safeConvert = mapM safeConvert
-}

{- | Convert from one type of data to another.  Raises an exception if there is
an error with the conversion.  For a function that does not raise an exception
in that case, see 'safeConvert'. -}
convert :: Convertible a b => a -> b
convert x = 
    case safeConvert x of
      Left e -> error (prettyConvertError e)
      Right r -> r

{-
instance Convertible Int Double where
    safeConvert = return . fromIntegral
instance Convertible Double Int where
    safeConvert = return . truncate         -- could do bounds checking here
instance Convertible Integer Double where
    safeConvert = return . fromIntegral
instance Convertible Double Integer where
    safeConvert = return . truncate
-}

----------------------------------------------------------------------
-- Error Handling
----------------------------------------------------------------------

{- | How we indicate that there was an error. -}
data ConvertError = ConvertError {
      convSourceValue :: String,
      convSourceType :: String,
      convDestType :: String,
      convErrorMessage :: String}
                    deriving (Eq, Read, Show)

instance Error ConvertError where
    strMsg x = ConvertError "(unknown)" "(unknown)" "(unknown)" x

convError' :: (Show a, Typeable a, Typeable b) =>
               String -> a -> b -> ConvertResult b
convError' msg inpval retval = 
     Left $ ConvertError {
             convSourceValue = show inpval,
             convSourceType = show . typeOf $ inpval,
             convDestType = show . typeOf $ retval,
             convErrorMessage = msg}
    
convError :: (Show a, Typeable a, Typeable b) =>
             String -> a -> ConvertResult b
convError msg inpval = 
    convError' msg inpval undefined
    
prettyConvertError :: ConvertError -> String
prettyConvertError (ConvertError sv st dt em) =
    "Convertible: error converting source data " ++ sv ++ " of type " ++ st
    ++ " to type " ++ dt ++ ": " ++ em
    
