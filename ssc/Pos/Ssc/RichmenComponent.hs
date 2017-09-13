{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.RichmenComponent
       ( RCSsc
       , getRichmenSsc
       ) where

import           Universum

import           Pos.Core               (EpochIndex, mpcThd, HasBlockVersionData)
import           Pos.DB.Class           (MonadDBRead)
import           Pos.Lrc.Class          (RichmenComponent (..))
import           Pos.Lrc.DB.RichmenBase (getRichmen)
import           Pos.Lrc.Types          (RichmenStakes)

data RCSsc

instance HasBlockVersionData => RichmenComponent RCSsc where
    type RichmenData RCSsc = RichmenStakes
    rcToData = snd
    rcTag Proxy = "ssc"
    rcInitialThreshold Proxy = mpcThd
    rcConsiderDelegated Proxy = True

getRichmenSsc :: (MonadDBRead m) => EpochIndex -> m (Maybe RichmenStakes)
getRichmenSsc = getRichmen @RCSsc
