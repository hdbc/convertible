{-# LANGUAGE DeriveDataTypeable #-}
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

module Data.Convertible.Base( -- * The conversion process
                              convert,
                              ConvertAttempt (..),
                              ConvertSuccess (..),
                              ConversionException (..),
                              convertAttemptWrap
                             )
where
import Data.Attempt
import Control.Exception (Exception)
import Data.Typeable (Typeable)

----------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------

{- | A typeclass that represents something that can attempt a conversion.
An @ConvertAttempt a b@ instance represents an @a@ that might be convertible to a @b@. -}
class ConvertAttempt a b where
    {- | Convert @a@ to @b@, returning 'Success' on success and 'Failure' on error.
     -}
    convertAttempt :: a -> Attempt b

{- | A typeclass that represents something that guarantees a successful conversion.
A @ConvertSuccess a b@ instance represents an @a@ that can be converted to a @b@. -}
class ConvertAttempt a b => ConvertSuccess a b where
    {- | Convert @a@ to @b@. -}
    convertSuccess :: a -> b

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
in that case, see 'convertAttempt'. -}
convert :: ConvertAttempt a b => a -> b
convert = fromSuccess . convertAttempt

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

{- | Wraps 'Exception' which could occur during a 'convertAttempt'.
-}
data ConversionException = forall e. Exception e => ConversionException e
    deriving Typeable
instance Show ConversionException where
    show (ConversionException e) = "ConversionException " ++ show e
instance Exception ConversionException

{- | Calls 'convertAttempt', wrapping any 'Exception's in a
 'ConversionException'
-}
convertAttemptWrap :: (ConvertAttempt a b,
                       MonadFailure ConversionException m
                      )
                   => a
                   -> m b
convertAttemptWrap = attempt (failure . ConversionException) return .
                     convertAttempt
