-- | Announcements related to blocks.

module Pos.Block.Network.Announce
       ( announceBlock
       ) where

import           Formatting                (build, sformat, (%))
import           System.Wlog               (logDebug)
import           Universum

import           Pos.Binary.Block.Network  ()
import           Pos.Block.Network.Types   (MsgHeaders (..))
import           Pos.Communication.Methods (sendToNeighborsSafeWithMaliciousEmulation)
import           Pos.Types                 (MainBlockHeader)
import           Pos.WorkMode              (WorkMode)

announceBlock
    :: WorkMode ssc m
    => MainBlockHeader ssc -> m ()
announceBlock header = do
    logDebug $ sformat ("Announcing header to others:\n"%build) header
    sendToNeighborsSafeWithMaliciousEmulation . MsgHeaders $ pure $ Right header
