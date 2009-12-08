{-
Copyright (C) 2009 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.Convertible.Instances
   Copyright  : Copyright (C) 2009 John Goerzen
   License    : LGPL

   Maintainer : Michael Snoyman <michael@snoyman.com>
   Stability  : provisional
   Portability: portable

Collection of ready-made 'ConvertSuccess' and 'ConvertAttempt' instances.  See
each individual module for more docs:

"Data.Convertible.Instances.C"

"Data.Convertible.Instances.Map"

"Data.Convertible.Instances.Num"

"Data.Convertible.Instances.Time"

"Data.Convertible.Instances.Text"

"Data.Convertible.Instances.String"

You can find a list of these instances at 'ConvertSuccess' and 'ConvertAttempt'.
-}

module Data.Convertible.Instances
    ( module Data.Convertible.Instances.String
    ) where

import Data.Convertible.Instances.C()
import Data.Convertible.Instances.Map()
import Data.Convertible.Instances.Num()
import Data.Convertible.Instances.Time()
import Data.Convertible.Instances.Text()
import Data.Convertible.Instances.String
