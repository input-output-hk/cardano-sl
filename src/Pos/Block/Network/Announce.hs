{-# LANGUAGE ScopedTypeVariables #-}
-- | Announcements related to blocks.

module Pos.Block.Network.Announce
       ( announceBlock
       , handleHeadersCommunication
       ) where

import           Formatting               (build, sformat, shown, (%))
import           Mockable                 (throw)
import           Node                     (ConversationActions (..), SendActions (..))
import           System.Wlog              (logDebug)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Logic          (getHeadersFromManyTo)
import           Pos.Block.Network.Types  (MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.BiP    (BiP)
import           Pos.Context              (getNodeContext, ncAttackTypes)
import           Pos.Crypto               (shortHashF)
import           Pos.DHT.Model            (converseToNeighbors, nodeIdToAddress)
import           Pos.Security             (AttackType (..), NodeAttackedError (..),
                                           shouldIgnoreAddress)
import           Pos.Types                (MainBlockHeader, headerHash)
import           Pos.WorkMode             (WorkMode)

announceBlock
    :: WorkMode ssc m
    => SendActions BiP m -> MainBlockHeader ssc -> m ()
announceBlock sendActions header = do
    logDebug $ sformat ("Announcing header to others:\n"%build) header
    cont <- getNodeContext
    let throwOnIgnored nId = whenJust (nodeIdToAddress nId) $
                                 \addr ->
                                   when (shouldIgnoreAddress cont addr) $
                                     throw AttackNoBlocksTriggered
        sendActions' =
          if AttackNoBlocks `elem` ncAttackTypes cont
             then sendActions { sendTo =
                                  \nId msg -> do
                                      throwOnIgnored nId
                                      sendTo sendActions nId msg
                              , withConnectionTo =
                                  \nId handler -> do
                                      throwOnIgnored nId
                                      withConnectionTo sendActions nId handler
                              }
             else sendActions
    converseToNeighbors sendActions' announceBlockDo
  where
    announceBlockDo nodeId conv = do
        logDebug $ sformat ("Announcing block "%shortHashF%" to "%shown) (headerHash header) nodeId
        send conv $ MsgHeaders (one (Right header))
        handleHeadersCommunication conv

handleHeadersCommunication
    :: WorkMode ssc m
    => ConversationActions (MsgHeaders ssc) MsgGetHeaders m
    -> m ()
handleHeadersCommunication conv = do
    (msg :: Maybe MsgGetHeaders) <- recv conv
    whenJust msg $ \(MsgGetHeaders {..}) -> do
        logDebug "Got request on handleGetHeaders"
        headers <- getHeadersFromManyTo mghFrom mghTo
        maybe onNoHeaders (send conv . MsgHeaders) headers
  where
    onNoHeaders =
        logDebug "getheadersFromManyTo returned Nothing, not replying to node"
