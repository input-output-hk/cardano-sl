{-# LANGUAGE ScopedTypeVariables #-}
-- | Announcements related to blocks.

module Pos.Block.Network.Announce
       ( announceBlock
       , announceBlockOuts
       , handleHeadersCommunication
       ) where

import           Data.Reflection            (Reifies)
import           Formatting                 (build, sformat, (%))
import           Mockable                   (throw)
import           System.Wlog                (logDebug)
import           Universum

import           Pos.Binary.Communication   ()
import           Pos.Block.Logic            (getHeadersFromManyTo)
import           Pos.Block.Network.Types    (MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Limits   (Limit, LimitedLength, recvLimited,
                                             reifyMsgLimit)
import           Pos.Communication.Message  ()
import           Pos.Communication.Protocol (ConversationActions (..), NodeId (..),
                                             OutSpecs, SendActions (..), convH,
                                             toOutSpecs)
import           Pos.Context                (getNodeContext, isRecoveryMode, ncNodeParams,
                                             npAttackTypes)
import           Pos.Crypto                 (shortHashF)
import qualified Pos.DB.Block               as DB
import qualified Pos.DB.DB                  as DB
import           Pos.Discovery              (converseToNeighbors)
import           Pos.Security               (AttackType (..), NodeAttackedError (..),
                                             shouldIgnoreAddress)
import           Pos.Types                  (MainBlockHeader, headerHash)
import           Pos.Util.TimeWarp          (nodeIdToAddress)
import           Pos.WorkMode               (WorkMode)

announceBlockOuts :: OutSpecs
announceBlockOuts = toOutSpecs [convH (Proxy :: Proxy (MsgHeaders ssc))
                                      (Proxy :: Proxy MsgGetHeaders)
                               ]

announceBlock
    :: WorkMode ssc m
    => m (Set NodeId) -> SendActions m -> MainBlockHeader ssc -> m ()
announceBlock getPeers sendActions header = do
    logDebug $ sformat ("Announcing header to others:\n"%build) header
    cont <- getNodeContext
    let throwOnIgnored (NodeId (_, nId)) =
            whenJust (nodeIdToAddress nId) $ \addr ->
                when (shouldIgnoreAddress cont addr) $
                throw AttackNoBlocksTriggered
        sendActions' =
            if AttackNoBlocks `elem` npAttackTypes (ncNodeParams cont)
                then sendActions
                     { sendTo =
                           \nId msg -> do
                               throwOnIgnored nId
                               sendTo sendActions nId msg
                     , withConnectionTo =
                           \nId handler -> do
                               throwOnIgnored nId
                               withConnectionTo sendActions nId handler
                     }
                else sendActions
    reifyMsgLimit (Proxy @MsgGetHeaders) $ \limitProxy -> do
        peers <- getPeers
        converseToNeighbors peers sendActions' $ announceBlockDo limitProxy
  where
    announceBlockDo limitProxy nodeId conv = do
        logDebug $
            sformat
                ("Announcing block "%shortHashF%" to "%build)
                (headerHash header)
                nodeId
        send conv $ MsgHeaders (one (Right header))
        handleHeadersCommunication conv limitProxy

handleHeadersCommunication
    :: forall ssc m s.
       (WorkMode ssc m, Reifies s (Limit MsgGetHeaders))
    => ConversationActions (MsgHeaders ssc) (LimitedLength s MsgGetHeaders) m
    -> Proxy s
    -> m ()
handleHeadersCommunication conv _ = do
    whenJustM (recvLimited conv) $ \mgh@(MsgGetHeaders {..}) -> do
        logDebug $ sformat ("Got request on handleGetHeaders: "%build) mgh
        ifM isRecoveryMode onRecovery $ do
            headers <- case (mghFrom,mghTo) of
                ([], Nothing) -> Just . one <$> DB.getTipBlockHeader @ssc
                ([], Just h)  -> fmap one <$> DB.getBlockHeader @ssc h
                (c1:cxs, _)   -> getHeadersFromManyTo (c1:|cxs) mghTo
            maybe onNoHeaders handleSuccess headers
  where
    handleSuccess h = do
        onSuccess
        send conv (MsgHeaders h)
        handleHeadersCommunication conv Proxy
    onSuccess =
        logDebug "handleGetHeaders: responded successfully"
    onRecovery =
        logDebug "handleGetHeaders: not responding, we're in recovery mode"
    onNoHeaders =
        logDebug "getheadersFromManyTo returned Nothing, not replying to node"
