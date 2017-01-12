-- | Announcements related to blocks.

module Pos.Block.Network.Announce
       ( announceBlock
       ) where

import           Formatting               (build, sformat, (%))
import           Node                     (SendActions (..))
import           System.Wlog              (logDebug)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Network.Types  (MsgHeaders (..))
import           Pos.Communication.BiP    (BiP)
import           Pos.Context              (getNodeContext, ncAttackTypes)
import           Pos.DHT.Model         (nodeIdToAddress, sendToNeighbors)
import           Pos.Security             (AttackType (..), shouldIgnoreAddress)
import           Pos.Types                (MainBlockHeader)
import           Pos.WorkMode             (WorkMode)

announceBlock
    :: WorkMode ssc m
    => SendActions BiP m -> MainBlockHeader ssc -> m ()
announceBlock sendActions header = do
    logDebug $ sformat ("Announcing header to others:\n"%build) header
    cont <- getNodeContext
    let sendActions' =
          if AttackNoBlocks `elem` ncAttackTypes cont
             then sendActions { sendTo =
                                  \nId msg -> whenJust (nodeIdToAddress nId) $
                                    \addr ->
                                      unless (shouldIgnoreAddress cont addr) $
                                          sendTo sendActions nId msg
                              }
             else sendActions
    -- [CSL-514] TODO Log long acting sends
    sendToNeighbors sendActions' . MsgHeaders $ pure $ Right header
