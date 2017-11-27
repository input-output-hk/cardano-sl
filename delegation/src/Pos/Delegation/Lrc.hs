-- | LRC-related part of the delegation system.

module Pos.Delegation.Lrc
       ( delegationLrcConsumer
       ) where

-- import           Universum

import           Pos.Core (BlockVersionData (bvdHeavyDelThd))
import qualified Pos.DB as DB
import           Pos.Delegation.RichmenComponent (RCDlg)
import           Pos.Lrc.Consumer (LrcConsumer (..), lrcConsumerFromComponentSimple)

-- | Consumer will be called on every Richmen computation.
delegationLrcConsumer :: (DB.MonadGState m, DB.MonadDB m) => LrcConsumer m
delegationLrcConsumer = lrcConsumerFromComponentSimple @RCDlg bvdHeavyDelThd
