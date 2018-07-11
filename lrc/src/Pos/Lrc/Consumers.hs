{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

module Pos.Lrc.Consumers
       ( allLrcConsumers
       ) where

import           Pos.Core (CoreConfiguration, HasGenesisBlockVersionData)
import           Pos.Lrc.Consumer (LrcConsumer)
import           Pos.Lrc.Consumer.Delegation (dlgLrcConsumer)
import           Pos.Lrc.Consumer.Ssc (sscLrcConsumer)
import           Pos.Lrc.Consumer.Update (usLrcConsumer)
import           Pos.Lrc.Mode (LrcMode)

allLrcConsumers
  :: (LrcMode ctx m, HasGenesisBlockVersionData)
  => CoreConfiguration -> [LrcConsumer m]
allLrcConsumers cc = [dlgLrcConsumer cc, usLrcConsumer cc, sscLrcConsumer cc]
