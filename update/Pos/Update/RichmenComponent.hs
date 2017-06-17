{-# LANGUAGE TypeFamilies #-}

module Pos.Update.RichmenComponent
       ( RCUs
       , getRichmenUS
       , putRichmenUS
       ) where

import           Universum

import           Pos.Core               (EpochIndex, genesisUpdateVoteThd)
import           Pos.DB.Class           (MonadDB, MonadDBRead)
import           Pos.Lrc.Class          (RichmenComponent (..))
import           Pos.Lrc.DB.RichmenBase (getRichmen, putRichmen)
import           Pos.Lrc.Types          (FullRichmenData)

data RCUs

instance RichmenComponent RCUs where
    type RichmenData RCUs = FullRichmenData
    rcToData = identity
    rcTag Proxy = "us"
    rcInitialThreshold Proxy = genesisUpdateVoteThd
    rcConsiderDelegated Proxy = True

getRichmenUS :: MonadDBRead m => EpochIndex -> m (Maybe FullRichmenData)
getRichmenUS = getRichmen @RCUs

putRichmenUS :: MonadDB m => EpochIndex -> FullRichmenData -> m ()
putRichmenUS = putRichmen @RCUs
