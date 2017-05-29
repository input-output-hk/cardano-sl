{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.RichmenComponent
       ( RCSsc
       , getRichmenSsc
       , putRichmenSsc
       ) where

import           Universum

import           Pos.Core               (EpochIndex)
import           Pos.DB.Class           (MonadDB)
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

getRichmenSsc :: MonadDB m => EpochIndex -> m (Maybe RichmenStake)
getRichmenSsc = getRichmen @RCSsc

putRichmenSsc
    :: (MonadDB m)
    => EpochIndex -> FullRichmenData -> m ()
putRichmenSsc = putRichmen @RCSsc
