-- | LRC-related part of the delegation system.

module Pos.Delegation.Lrc
       ( delegationLrcConsumer
       ) where

-- import           Universum

import qualified Pos.DB           as DB
import           Pos.Lrc.Consumer (LrcConsumer (..), lrcConsumerFromComponentSimple)
import           Pos.Lrc.DB       (RCDlg)

-- | Consumer will be called on every Richmen computation.
delegationLrcConsumer :: DB.MonadDB m => LrcConsumer m
delegationLrcConsumer = lrcConsumerFromComponentSimple @RCDlg
