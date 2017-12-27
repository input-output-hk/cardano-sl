{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.RichmenComponent
       ( RCSsc
       , getRichmenSsc
       ) where

import           Universum

import           Pos.Core (BlockVersionData (bvdMpcThd), EpochIndex, HasGenesisBlockVersionData,
                           genesisBlockVersionData)
import           Pos.DB.Class (MonadDBRead)
import           Pos.Lrc.DB.RichmenBase (getRichmen)
import           Pos.Lrc.RichmenComponent (RichmenComponent (..))
import           Pos.Lrc.Types (RichmenStakes)

data RCSsc

instance HasGenesisBlockVersionData => RichmenComponent RCSsc where
    type RichmenData RCSsc = RichmenStakes
    rcToData = snd
    rcTag Proxy = "ssc"
    rcInitialThreshold Proxy = bvdMpcThd genesisBlockVersionData
    rcConsiderDelegated Proxy = True

getRichmenSsc :: (MonadDBRead m) => EpochIndex -> m (Maybe RichmenStakes)
getRichmenSsc = getRichmen @RCSsc
