{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.RichmenComponent
       ( RCSsc
       , getRichmenSsc
       ) where

import           Universum

import           Pos.Core               (EpochIndex, genesisMpcThd)
import           Pos.DB.Class           (MonadDBRead)
import           Pos.Lrc.Class          (RichmenComponent (..))
import           Pos.Lrc.DB.RichmenBase (getRichmen)
import           Pos.Lrc.Types          (RichmenStakes)

data RCSsc

instance RichmenComponent RCSsc where
    type RichmenData RCSsc = RichmenStakes
    rcToData = snd
    rcTag Proxy = "ssc"
    rcInitialThreshold Proxy = genesisMpcThd
    rcConsiderDelegated Proxy = True

getRichmenSsc :: (MonadDBRead m) => EpochIndex -> m (Maybe RichmenStakes)
getRichmenSsc = getRichmen @RCSsc
