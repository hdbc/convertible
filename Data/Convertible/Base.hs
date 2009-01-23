{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.Convertible.Base
   Copyright  : Copyright (C) 2009 John Goerzen
   License    : LGPL

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

-}

module Data.Convertible.Base (-- * Handling the results
                              ConvertResult,
                              -- * The conversion process
                              convert,
                              Convertible(..),
                              -- * Error handling
                              ConvertError(..),
                              convError,
                              ConvTypeName(..),
                              prettyConvertError
                             )
where
import Control.Monad.Error
import Data.Typeable

{- | The result of a safe conversion via 'safeConvert'. -}
type ConvertResult a = Either ConvertError a

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

{- | Because we might not always want to define Typeable instances, we can use this
to get the name of something.

This function usually will ignore its paraneter, and should be fine if passed undefined. -}
class ConvTypeName a where
    convTypeName :: a -> String

instance (Typeable a) => ConvTypeName a where
    convTypeName = show . typeOf

convError' :: (Show a, ConvTypeName a, ConvTypeName b) =>
               String -> a -> b -> ConvertResult b
convError' msg inpval retval = 
     Left $ ConvertError {
             convSourceValue = show inpval,
             convSourceType = convTypeName inpval,
             convDestType = convTypeName retval,
             convErrorMessage = msg}
    
convError :: (Show a, ConvTypeName a, ConvTypeName b) =>
             String -> a -> ConvertResult b
convError msg inpval = 
    convError' msg inpval undefined
    
prettyConvertError :: ConvertError -> String
prettyConvertError (ConvertError sv st dt em) =
    "Convertible: error converting source data " ++ sv ++ " of type " ++ st
    ++ " to type " ++ dt ++ ": " ++ em
    
