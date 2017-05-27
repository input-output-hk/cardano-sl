{-# LANGUAGE ScopedTypeVariables #-}
-- | Announcements related to blocks.

module Pos.Block.Network.Announce
       ( announceBlock
       , announceBlockOuts
       , handleHeadersCommunication
       ) where

import           Universum

import           Control.Monad.Except       (runExceptT)
import           Data.Reflection            (Reifies)
import qualified Ether
import           Formatting                 (build, sformat, (%))
import           Mockable                   (throw)
import           System.Wlog                (logDebug)

import           Pos.Binary.Communication   ()
import           Pos.Block.Core             (Block, BlockHeader, MainBlockHeader,
                                             blockHeader)
import           Pos.Block.Logic            (getHeadersFromManyTo)
import           Pos.Block.Network.Types    (MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Limits   (Limit, LimitedLength, recvLimited,
                                             reifyMsgLimit)
import           Pos.Communication.Message  ()
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             OutSpecs, SendActions (..), convH,
                                             toOutSpecs)
import           Pos.Context                (NodeParams, npAttackTypes,
                                             recoveryInProgress)
import           Pos.Core                   (headerHash, prevBlockL)
import           Pos.Crypto                 (shortHashF)
import qualified Pos.DB.Block               as DB
import qualified Pos.DB.DB                  as DB
import           Pos.Discovery              (converseToNeighbors)
import           Pos.Security               (AttackType (..), NodeAttackedError (..),
                                             shouldIgnoreAddress)
import           Pos.Util.TimeWarp          (nodeIdToAddress)
import           Pos.WorkMode.Class         (WorkMode)

announceBlockOuts :: OutSpecs
announceBlockOuts = toOutSpecs [convH (Proxy :: Proxy (MsgHeaders ssc))
                                      (Proxy :: Proxy MsgGetHeaders)
                               ]

announceBlock
    :: WorkMode ssc m
    => SendActions m -> MainBlockHeader ssc -> m ()
announceBlock sendActions header = do
    logDebug $ sformat ("Announcing header to others:\n"%build) header
    nodeParams <- Ether.ask @NodeParams
    let throwOnIgnored nId =
            whenJust (nodeIdToAddress nId) $ \addr ->
                when (shouldIgnoreAddress nodeParams addr) $
                throw AttackNoBlocksTriggered
        sendActions' =
            if AttackNoBlocks `elem` npAttackTypes nodeParams
                then sendActions
                     { withConnectionTo =
                           \nId handler -> do
                               throwOnIgnored nId
                               withConnectionTo sendActions nId handler
                     }
                else sendActions
    reifyMsgLimit (Proxy @MsgGetHeaders) $ \limitProxy -> do
        converseToNeighbors sendActions' $ announceBlockDo limitProxy
  where
    announceBlockDo limitProxy nodeId = pure $ Conversation $ \cA -> do
        logDebug $
            sformat
                ("Announcing block "%shortHashF%" to "%build)
                (headerHash header)
                nodeId
        send cA $ MsgHeaders (one (Right header))
        handleHeadersCommunication cA limitProxy

handleHeadersCommunication
    :: forall ssc m s.
       (WorkMode ssc m, Reifies s (Limit MsgGetHeaders))
    => ConversationActions (MsgHeaders ssc) (LimitedLength s MsgGetHeaders) m
    -> Proxy s
    -> m ()
handleHeadersCommunication conv _ = do
    whenJustM (recvLimited conv) $ \mgh@(MsgGetHeaders {..}) -> do
        logDebug $ sformat ("Got request on handleGetHeaders: "%build) mgh
        ifM recoveryInProgress onRecovery $ do
            headers <- case (mghFrom,mghTo) of
                ([], Nothing) -> Right . one <$> getLastMainHeader
                ([], Just h)  ->
                    maybeToRight "getBlockHeader returned Nothing" . fmap one <$>
                    DB.blkGetHeader @ssc h
                (c1:cxs, _)   -> runExceptT
                    (getHeadersFromManyTo (c1:|cxs) mghTo)
            either onNoHeaders handleSuccess headers
  where
    -- retrieves header of the newest main block if there's any,
    -- genesis otherwise.
    getLastMainHeader :: m (BlockHeader ssc)
    getLastMainHeader = do
        (tip :: Block ssc) <- DB.getTipBlock @(Block ssc)
        let tipHeader = tip ^. blockHeader
        case tip of
            Left _  -> fromMaybe tipHeader <$> DB.blkGetHeader (tip ^. prevBlockL)
            Right _ -> pure tipHeader
    handleSuccess h = do
        onSuccess
        send conv (MsgHeaders h)
        handleHeadersCommunication conv Proxy
    onSuccess =
        logDebug "handleGetHeaders: responded successfully"
    onRecovery =
        logDebug "handleGetHeaders: not responding, we're in recovery mode"
    onNoHeaders reason =
        logDebug $ "getheadersFromManyTo returned Nothing, " <>
                   "not replying to node, reason: " <> reason
