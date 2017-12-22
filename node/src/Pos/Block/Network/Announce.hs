{-# LANGUAGE RankNTypes #-}

-- | Announcements related to blocks.

module Pos.Block.Network.Announce
       ( announceBlock
       , announceBlockOuts
       , handleHeadersCommunication
       ) where

import           Universum

import           Control.Monad.Except       (runExceptT)
import           Ether.Internal             (HasLens (..))
import           Formatting                 (build, sformat, (%))
import           Mockable                   (throw)
import           System.Wlog                (logDebug, logWarning)

import           Pos.Block.Core             (Block, BlockHeader, MainBlockHeader,
                                             blockHeader)
import           Pos.Block.Logic            (getHeadersFromManyTo)
import           Pos.Block.Network.Types    (MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Limits   (recvLimited)
import           Pos.Communication.Message  ()
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             EnqueueMsg, MsgType (..), NodeId,
                                             Origin (..), OutSpecs, convH, toOutSpecs)
import           Pos.Context                (recoveryInProgress)
import           Pos.Core                   (headerHash, prevBlockL)
import           Pos.Crypto                 (shortHashF)
import qualified Pos.DB.Block               as DB
import qualified Pos.DB.DB                  as DB
import           Pos.Security               (AttackType (..), NodeAttackedError (..),
                                             SecurityParams (..), shouldIgnoreAddress)
import           Pos.Util.TimeWarp          (nodeIdToAddress)
import           Pos.WorkMode.Class         (WorkMode)

announceBlockOuts :: OutSpecs
announceBlockOuts = toOutSpecs [convH (Proxy :: Proxy (MsgHeaders ssc))
                                      (Proxy :: Proxy MsgGetHeaders)
                               ]

announceBlock
    :: WorkMode ssc ctx m
    => EnqueueMsg m -> MainBlockHeader ssc -> m (Map NodeId (m ()))
announceBlock enqueue header = do
    logDebug $ sformat ("Announcing header to others:\n"%build) header
    enqueue (MsgAnnounceBlockHeader OriginSender) (\addr _ -> announceBlockDo addr)
  where
    announceBlockDo nodeId = pure $ Conversation $ \cA -> do
        SecurityParams{..} <- view (lensOf @SecurityParams)
        let throwOnIgnored nId =
                whenJust (nodeIdToAddress nId) $ \addr ->
                    whenM (shouldIgnoreAddress addr) $
                        throw AttackNoBlocksTriggered
        when (AttackNoBlocks `elem` spAttackTypes) (throwOnIgnored nodeId)
        logDebug $
            sformat
                ("Announcing block "%shortHashF%" to "%build)
                (headerHash header)
                nodeId
        send cA $ MsgHeaders (one (Right header))
        handleHeadersCommunication cA

handleHeadersCommunication
    :: forall ssc ctx m .
       (WorkMode ssc ctx m)
    => ConversationActions (MsgHeaders ssc) MsgGetHeaders m
    -> m ()
handleHeadersCommunication conv = do
    whenJustM (recvLimited conv) $ \mgh@(MsgGetHeaders {..}) -> do
        logDebug $ sformat ("Got request on handleGetHeaders: "%build) mgh
        ifM recoveryInProgress onRecovery $ do
            headers <- case (mghFrom,mghTo) of
                ([], Nothing) -> Right . one <$> getLastMainHeader
                ([], Just h)  ->
                    maybeToRight "getBlockHeader returned Nothing" . fmap one <$>
                    DB.blkGetHeader @ssc h
                (c1:cxs, _)   ->
                    first ("getHeadersFromManyTo: " <>) <$>
                    runExceptT (getHeadersFromManyTo (c1:|cxs) mghTo)
            either onNoHeaders handleSuccess headers
  where
    -- retrieves header of the newest main block if there's any,
    -- genesis otherwise.
    getLastMainHeader :: m (BlockHeader ssc)
    getLastMainHeader = do
        (tip :: Block ssc) <- DB.getTipBlock
        let tipHeader = tip ^. blockHeader
        case tip of
            Left _  -> fromMaybe tipHeader <$> DB.blkGetHeader (tip ^. prevBlockL)
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
