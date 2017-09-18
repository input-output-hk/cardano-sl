{-# LANGUAGE TypeFamilies #-}

module Pos.Update.RichmenComponent
       ( RCUs
       , getRichmenUS
       ) where

import           Universum

import           Pos.Core               (EpochIndex, updateVoteThd, HasBlockVersionData)
import           Pos.DB.Class           (MonadDBRead)
import           Pos.Lrc.Class          (RichmenComponent (..))
import           Pos.Lrc.DB.RichmenBase (getRichmen)
import           Pos.Lrc.Types          (FullRichmenData)

data RCUs

instance HasBlockVersionData => RichmenComponent RCUs where
    type RichmenData RCUs = FullRichmenData
    rcToData = identity
    rcTag Proxy = "us"
    rcInitialThreshold Proxy = updateVoteThd
    rcConsiderDelegated Proxy = True

getRichmenUS :: MonadDBRead m => EpochIndex -> m (Maybe FullRichmenData)
getRichmenUS = getRichmen @RCUs
