{-# LANGUAGE TypeFamilies #-}

module Pos.Update.RichmenComponent
       ( RCUs
       , getRichmenUS
       ) where

import           Universum

import           Pos.Core (BlockVersionData (bvdUpdateVoteThd), EpochIndex,
                           HasGenesisBlockVersionData, genesisBlockVersionData)
import           Pos.DB.Class (MonadDBRead)
import           Pos.Lrc.DB.RichmenBase (getRichmen)
import           Pos.Lrc.RichmenComponent (RichmenComponent (..))
import           Pos.Lrc.Types (FullRichmenData)

data RCUs

instance HasGenesisBlockVersionData => RichmenComponent RCUs where
    type RichmenData RCUs = FullRichmenData
    rcToData = identity
    rcTag Proxy = "us"
    rcInitialThreshold Proxy = bvdUpdateVoteThd genesisBlockVersionData
    rcConsiderDelegated Proxy = True

getRichmenUS :: MonadDBRead m => EpochIndex -> m (Maybe FullRichmenData)
getRichmenUS = getRichmen @RCUs
