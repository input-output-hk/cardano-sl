{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Abstract parts of LRC.

module Pos.Lrc.RichmenComponent
       ( RichmenComponent (..)
       , SomeRichmenComponent (..)
       , someRichmenComponent
       ) where

import           Universum

import           Pos.Binary.Class (Bi)
import           Pos.Core.Common (CoinPortion)
import           Pos.Lrc.Types (FullRichmenData)

----------------------------------------------------------------------------
-- Class
----------------------------------------------------------------------------

-- | Class for components that store info about richmen.
class Bi (RichmenData a) => RichmenComponent a where
    -- | Datatype that is stored. Consider using 'RichmenSet' or
    -- 'RichmenStakes' or 'FullRichmenData'.
    type RichmenData a :: *
    -- | Converts 'FullRichmenData' to what is needs to be saved.
    rcToData :: FullRichmenData -> RichmenData a
    -- | Tag to identify component (short bytestring).
    rcTag :: Proxy a -> ByteString
    -- | Threshold used to deliminate richmen initially. Argument is
    -- the total system stake.
    rcInitialThreshold :: Proxy a -> CoinPortion
    -- | Whether to consider delegated stake.
    rcConsiderDelegated :: Proxy a -> Bool


data SomeRichmenComponent =
    forall c. (RichmenComponent c) =>
              SomeRichmenComponent (Proxy c)

someRichmenComponent
    :: forall c.
       RichmenComponent c
    => SomeRichmenComponent
someRichmenComponent = SomeRichmenComponent (Proxy @c)
