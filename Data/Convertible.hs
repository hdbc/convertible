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

This is a library to provide a uniform interface for safe conversions between
different types of data.  To get started reading about it, consult:

"Data.Convertible.Base" for information about the conversions themselves

"Data.Convertible.Utils" for helpful tools for people writing 'Convertible' instances

"Data.Convertible.Instances" for a large collection of ready-built 'Convertible' instances

You can import these modules individually, or this module will export the entire library
for you.
-}

module Data.Convertible (
                         module Data.Convertible.Base,
                         module Data.Convertible.Utils,
                         module Data.Convertible.Instances
                         )
where
import Data.Convertible.Base
import Data.Convertible.Utils
import Data.Convertible.Instances
