{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

module Pos.Lrc.Consumers
       (
         allLrcConsumers
       ) where

import           Pos.Delegation.Lrc (dlgLrcConsumer)
import           Pos.Lrc.Consumer (LrcConsumer)
import           Pos.Lrc.Mode (LrcMode)
import           Pos.Ssc.Lrc (sscLrcConsumer)
import           Pos.Ssc.Message (SscMessageConstraints)
import           Pos.Update.Lrc (usLrcConsumer)

allLrcConsumers
    :: forall ctx m. (SscMessageConstraints, LrcMode ctx m)
    => [LrcConsumer m]
allLrcConsumers = [dlgLrcConsumer, usLrcConsumer, sscLrcConsumer]
