{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

module Pos.DB.Lrc.Consumers
       ( allLrcConsumers
       ) where

import           Universum

import           Pos.Chain.Update (BlockVersionData)
import           Pos.DB.Lrc.Consumer (LrcConsumer)
import           Pos.DB.Lrc.Consumer.Delegation (dlgLrcConsumer)
import           Pos.DB.Lrc.Consumer.Ssc (sscLrcConsumer)
import           Pos.DB.Lrc.Consumer.Update (usLrcConsumer)
import           Pos.DB.Lrc.Mode (LrcMode)

allLrcConsumers :: LrcMode ctx m => BlockVersionData -> [LrcConsumer m]
allLrcConsumers genesisBvd =
    ($ genesisBvd) <$> [dlgLrcConsumer, usLrcConsumer, sscLrcConsumer]
