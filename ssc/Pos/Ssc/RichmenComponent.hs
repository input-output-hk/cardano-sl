{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.RichmenComponent
       ( RCSsc
       , getRichmenSsc
       , putRichmenSsc
       ) where

import           Universum

import           Pos.Core               (EpochIndex)
import           Pos.DB.Class           (MonadRealDB)
import           Pos.Lrc.Class          (RichmenComponent (..))
import           Pos.Lrc.DB.RichmenBase (getRichmen, putRichmen)
import           Pos.Lrc.Types          (FullRichmenData, RichmenStake)
import           Pos.Update.Constants   (genesisMpcThd)

data RCSsc

instance RichmenComponent RCSsc where
    type RichmenData RCSsc = RichmenStake
    rcToData = snd
    rcTag Proxy = "ssc"
    rcInitialThreshold Proxy = genesisMpcThd
    rcConsiderDelegated Proxy = True

getRichmenSsc :: MonadRealDB m => EpochIndex -> m (Maybe RichmenStake)
getRichmenSsc = getRichmen @RCSsc

putRichmenSsc
    :: (MonadRealDB m)
    => EpochIndex -> FullRichmenData -> m ()
putRichmenSsc = putRichmen @RCSsc
