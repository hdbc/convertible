{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.Convertible.Base
   Copyright  : Copyright (C) 2009 John Goerzen
   License    : LGPL

   Maintainer : Michael Snoyman <michael@snoyman.com>
   Stability  : provisional
   Portability: portable

-}

module Data.Convertible.Base( ConvertAttempt (..),
                              ConvertSuccess (..),
                              ConversionException (..),
                              convertUnsafe,
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

instance ConvertSuccess a b => ConvertAttempt a b where
    convertAttempt = return . convertSuccess

{- | Any type can be converted to itself. -}
instance ConvertSuccess a a where
    convertSuccess = id

{-
{- | Lists of any convertible type can be converted. -}
instance Convertible a b => Convertible [a] [b] where
    safeConvert = mapM safeConvert
-}

{- | Convert from one type of data to another.  Raises an exception if there is
an error with the conversion.  For a function that does not raise an exception
in that case, see 'convertAttempt'. -}
convertUnsafe :: ConvertAttempt a b => a -> b
convertUnsafe = fromSuccess . convertAttempt

{- | Wraps any 'Exception' which could occur during a 'convertAttempt'.
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
