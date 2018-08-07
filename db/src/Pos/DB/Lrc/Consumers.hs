{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

module Pos.DB.Lrc.Consumers
       ( allLrcConsumers
       ) where

import           Pos.DB.Lrc.Consumer (LrcConsumer)
import           Pos.DB.Lrc.Consumer.Delegation (dlgLrcConsumer)
import           Pos.DB.Lrc.Consumer.Ssc (sscLrcConsumer)
import           Pos.DB.Lrc.Consumer.Update (usLrcConsumer)
import           Pos.DB.Lrc.Mode (LrcMode)

allLrcConsumers :: LrcMode ctx m => [LrcConsumer m]
allLrcConsumers = [dlgLrcConsumer, usLrcConsumer, sscLrcConsumer]
