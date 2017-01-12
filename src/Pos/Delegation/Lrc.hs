-- | LRC-related part of the delegation system.

module Pos.Delegation.Lrc
       ( delegationLrcConsumer
       ) where

-- import           Universum

import qualified Pos.DB           as DB
import           Pos.DB.Lrc       (RCDlg)
import           Pos.Lrc.Consumer (LrcConsumer (..), lrcConsumerFromComponentSimple)

-- | Consumer will be called on every Richmen computation.
delegationLrcConsumer :: DB.MonadDB ssc m => LrcConsumer m
delegationLrcConsumer = lrcConsumerFromComponentSimple @RCDlg
