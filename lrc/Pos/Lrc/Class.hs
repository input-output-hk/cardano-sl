{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Abstract parts of LRC.

module Pos.Lrc.Class
       ( RichmenComponent (..)
       , SomeRichmenComponent (..)
       , someRichmenComponent
       ) where

import           Universum

import           Pos.Binary.Class (Bi)
import           Pos.Core.Types   (Coin)
import           Pos.Lrc.Types    (FullRichmenData)


----------------------------------------------------------------------------
-- Class
----------------------------------------------------------------------------

-- | Class for components that store info about richmen.
class Bi (RichmenData a) => RichmenComponent a where
    -- | Datatype that is stored. Consider using 'Richmen' or
    -- 'RichmenStake' or 'FullRichmenData'.
    type RichmenData a :: *
    -- | Converts 'FullRichmenData' to what is needs to be saved.
    rcToData :: FullRichmenData -> RichmenData a
    -- | Tag to identify component (short bytestring).
    rcTag :: Proxy a -> ByteString
    -- | Threshold for the richman. Argument is total system stake.
    rcThreshold :: Proxy a -> Coin -> Coin
    -- | Whether to consider delegated stake.
    rcConsiderDelegated :: Proxy a -> Bool


data SomeRichmenComponent =
    forall c. (RichmenComponent c) =>
              SomeRichmenComponent (Proxy c)

someRichmenComponent
    :: forall c.
       RichmenComponent c
    => SomeRichmenComponent
someRichmenComponent = SomeRichmenComponent (Proxy :: Proxy c)
