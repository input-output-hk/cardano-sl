{-# LANGUAGE ScopedTypeVariables #-}
-- | Announcements related to blocks.

module Pos.Block.Network.Announce
       ( announceBlock
       , announceBlockOuts
       , handleHeadersCommunication
       ) where

import           Universum

import           Control.Monad.Except       (runExceptT)
import           EtherCompat
import           Formatting                 (build, sformat, (%))
import           Mockable                   (throw)
import           System.Wlog                (logDebug)

import           Pos.Binary.Communication   ()
import           Pos.Block.Core             (Block, BlockHeader, MainBlockHeader,
                                             blockHeader)
import           Pos.Block.Logic            (getHeadersFromManyTo)
import           Pos.Block.Network.Types    (MsgGetHeaders (..), MsgHeaders (..))
import           Pos.Communication.Limits   (recvLimited)
import           Pos.Communication.Message  ()
import           Pos.Communication.Protocol (Conversation (..), ConversationActions (..),
                                             OutSpecs, SendActions (..), convH,
                                             toOutSpecs)
import           Pos.Context                (recoveryInProgress)
import           Pos.Core                   (headerHash, prevBlockL)
import           Pos.Crypto                 (shortHashF)
import qualified Pos.DB.Block               as DB
import qualified Pos.DB.DB                  as DB
import           Pos.Discovery              (converseToNeighbors)
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
    => SendActions m -> MainBlockHeader ssc -> m ()
announceBlock sendActions header = do
    logDebug $ sformat ("Announcing header to others:\n"%build) header
    SecurityParams{..} <- view (lensOf @SecurityParams)
    let throwOnIgnored nId =
            whenJust (nodeIdToAddress nId) $ \addr ->
                whenM (shouldIgnoreAddress addr) $
                    throw AttackNoBlocksTriggered
        sendActions' =
            if AttackNoBlocks `elem` spAttackTypes
                then sendActions
                     { withConnectionTo =
                           \nId handler -> do
                               throwOnIgnored nId
                               withConnectionTo sendActions nId handler
                     }
                else sendActions
    converseToNeighbors sendActions' announceBlockDo
  where
    announceBlockDo nodeId = pure $ Conversation $ \cA -> do
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
                (c1:cxs, _)   -> runExceptT
                    (getHeadersFromManyTo (c1:|cxs) mghTo)
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
        onSuccess
        send conv (MsgHeaders h)
        handleHeadersCommunication conv
    onSuccess =
        logDebug "handleGetHeaders: responded successfully"
    onRecovery =
        logDebug "handleGetHeaders: not responding, we're in recovery mode"
    onNoHeaders reason =
        logDebug $ "getheadersFromManyTo returned Nothing, " <>
                   "not replying to node, reason: " <> reason
