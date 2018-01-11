{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | Announcements related to blocks.

module Pos.Block.Network.Announce
       ( announceBlock
       , announceBlockOuts
       , handleHeadersCommunication
       ) where

import           Universum

import           Control.Monad.Except (runExceptT)
import           Ether.Internal (lensOf)
import           Formatting (build, sformat, (%))
import           System.Wlog (logDebug, logWarning)

import           Pos.Block.BlockWorkMode (BlockWorkMode)
import           Pos.Block.Logic (getHeadersFromManyTo)
import           Pos.Block.Network.Types (MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Limits.Types (recvLimited)
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             EnqueueMsg, Message, MsgType (..), NodeId, Origin (..),
                                             OutSpecs, convH, toOutSpecs)
import           Pos.Core (headerHash, prevBlockL)
import           Pos.Core.Block (Block, BlockHeader, MainBlockHeader, blockHeader)
import           Pos.Crypto (shortHashF)
import qualified Pos.DB.Block as DB
import qualified Pos.DB.BlockIndex as DB
import           Pos.Recovery.Info (recoveryInProgress)
import           Pos.Security.Params (AttackType (..), NodeAttackedError (..), SecurityParams (..))
import           Pos.Security.Util (shouldIgnoreAddress)
import           Pos.Util.TimeWarp (nodeIdToAddress)

announceBlockOuts :: (Message MsgGetHeaders, Message MsgHeaders) => OutSpecs
announceBlockOuts = toOutSpecs [convH (Proxy :: Proxy MsgHeaders)
                                      (Proxy :: Proxy MsgGetHeaders)
                               ]

announceBlock
    :: BlockWorkMode ctx m
    => EnqueueMsg m -> MainBlockHeader -> m (Map NodeId (m ()))
announceBlock enqueue header = do
    logDebug $ sformat ("Announcing header to others: "%shortHashF)
               (headerHash header)
    enqueue (MsgAnnounceBlockHeader OriginSender) (\addr _ -> announceBlockDo addr)
  where
    announceBlockDo
        :: BlockWorkMode ctx m
        => NodeId -> NonEmpty (Conversation m ())
    announceBlockDo nodeId = pure $ Conversation $ \cA -> do
        SecurityParams{..} <- view (lensOf @SecurityParams)
        let throwOnIgnored nId =
                whenJust (nodeIdToAddress nId) $ \addr ->
                    whenM (shouldIgnoreAddress addr) $
                        throwM AttackNoBlocksTriggered
        when (AttackNoBlocks `elem` spAttackTypes) (throwOnIgnored nodeId)
        logDebug $
            sformat
                ("Announcing block "%shortHashF%" to "%build)
                (headerHash header)
                nodeId
        send cA $ MsgHeaders (one (Right header))
        handleHeadersCommunication cA

handleHeadersCommunication
    :: BlockWorkMode ctx m
    => ConversationActions MsgHeaders MsgGetHeaders m
    -> m ()
handleHeadersCommunication conv = do
    whenJustM (recvLimited conv) $ \mgh@(MsgGetHeaders {..}) -> do
        logDebug $ sformat ("Got request on handleGetHeaders: "%build) mgh
        ifM recoveryInProgress onRecovery $ do
            headers <- case (mghFrom,mghTo) of
                ([], Nothing) -> Right . one <$> getLastMainHeader
                ([], Just h)  ->
                    maybeToRight "getBlockHeader returned Nothing" . fmap one <$>
                    DB.getHeader h
                (c1:cxs, _)   ->
                    first ("getHeadersFromManyTo: " <>) <$>
                    runExceptT (getHeadersFromManyTo (c1:|cxs) mghTo)
            either onNoHeaders handleSuccess headers
  where
    -- retrieves header of the newest main block if there's any,
    -- genesis otherwise.
    getLastMainHeader :: BlockWorkMode ctx m => m BlockHeader
    getLastMainHeader = do
        tip :: Block <- DB.getTipBlock
        let tipHeader = tip ^. blockHeader
        case tip of
            Left _  -> fromMaybe tipHeader <$> DB.getHeader (tip ^. prevBlockL)
            Right _ -> pure tipHeader
    handleSuccess h = do
        send conv (MsgHeaders h)
        logDebug "handleGetHeaders: responded successfully"
        handleHeadersCommunication conv
    onNoHeaders reason = do
        let err = "handleGetHeaders: couldn't retrieve headers, reason: " <> reason
        logWarning err
        send conv (MsgNoHeaders err)
    onRecovery = do
        logDebug "handleGetHeaders: not responding, we're in recovery mode"
        send conv (MsgNoHeaders "server node is in recovery mode")
