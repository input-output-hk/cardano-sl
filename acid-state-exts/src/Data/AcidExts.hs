-----------------------------------------------------------------------------
{- |
 Module      :  Data.AcidExts
 Copyright   :  PublicDomain

 Maintainer  :  lemmih@gmail.com
 Portability :  non-portable (uses GHC extensions)

 AcidState container using a transaction log on disk.

 To see how it all fits together, have a look at these example
 <https://github.com/acid-state/acid-state/tree/master/examples>.

-}

module Data.AcidExts
    ( module Acid
    ) where

import Data.Acid as Acid
