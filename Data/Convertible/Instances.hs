{-
Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE

-}

{- |
   Module     : Data.Convertible.Instances
   Copyright  : Copyright (C) 2009-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Collection of ready-made 'Data.Convertible.Convertible' instances.  See
each individual module for more docs:

"Data.Convertible.Instances.C"

"Data.Convertible.Instances.Map"

"Data.Convertible.Instances.Num"

"Data.Convertible.Instances.Time"

You can find a list of these instances at 'Data.Convertible.Base.Convertible'.
-}

module Data.Convertible.Instances(
                                 ) where

import Data.Convertible.Instances.C()
import Data.Convertible.Instances.Map()
import Data.Convertible.Instances.Num()
import Data.Convertible.Instances.Text()
import Data.Convertible.Instances.Time()
