{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

module Pos.Lrc.Consumers
       ( allLrcConsumers
       ) where

import           Pos.Lrc.Consumer (LrcConsumer)
import           Pos.Lrc.Consumer.Delegation (dlgLrcConsumer)
import           Pos.Lrc.Consumer.Ssc (sscLrcConsumer)
import           Pos.Lrc.Consumer.Update (usLrcConsumer)
import           Pos.Lrc.Mode (LrcMode)

allLrcConsumers :: LrcMode ctx m => [LrcConsumer m]
allLrcConsumers = [dlgLrcConsumer, usLrcConsumer, sscLrcConsumer]
