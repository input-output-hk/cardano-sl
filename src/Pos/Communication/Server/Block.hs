{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Server which handles blocks.

module Pos.Communication.Server.Block
       ( blockListeners
       ) where

import           Control.Lens              ((^.))
import qualified Data.HashMap.Strict       as HM
import qualified Data.HashSet              as HS
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import qualified Data.List.NonEmpty        as NE
import           Data.Maybe                (fromJust)
import           Formatting                (bprint, build, int, sformat, stext, (%))
import           Serokell.Util             (VerificationRes (..), listJson,
                                            listJsonIndent)
import           System.Wlog               (logDebug, logError, logInfo, logNotice,
                                            logWarning)
import           Universum

import           Pos.Binary.Communication  ()
import           Pos.Communication.Methods (announceBlock)
import           Pos.Communication.Types   (MsgBlock (..), MutSocketState,
                                            RequestBlock (..), RequestBlockchainPart (..),
                                            ResponseMode, SendBlockHeader (..),
                                            SendBlockchainPart (..))
import           Pos.Context               (getNodeContext, ncPropagation)
import           Pos.Crypto                (hash, shortHashF)
import           Pos.DHT.Model             (ListenerDHT (..), MonadDHTDialog, replyToNode)
import           Pos.Slotting              (getCurrentSlot)
import           Pos.Ssc.Class.LocalData   (sscApplyGlobalState)
import qualified Pos.State                 as St
import           Pos.Txp.LocalData         (getLocalTxs, txApplyHeadUtxo,
                                            txLocalDataRollback)
import           Pos.Types                 (HeaderHash, Tx, blockTxs, getBlockHeader,
                                            headerHash)
import           Pos.Util                  (inAssertMode)
import           Pos.Util.JsonLog          (jlAdoptedBlock, jlLog)
import           Pos.WorkMode              (WorkMode)

-- | Listeners for requests related to blocks processing.
blockListeners
    :: (MonadDHTDialog (MutSocketState ssc) m, WorkMode ssc m)
    => [ListenerDHT (MutSocketState ssc) m]
blockListeners =
    [ ListenerDHT handleBlock
    , ListenerDHT handleBlockHeader
    , ListenerDHT handleBlockRequest
    , ListenerDHT handleBlockchainPartRequest
    ]

-- | Handler 'MsgBlock' event.
handleBlock
    :: forall ssc m.
       (ResponseMode ssc m)
    => MsgBlock ssc -> m ()
handleBlock (MsgBlock block) = do
    slotId <- getCurrentSlot
    localTxs <- HM.toList <$> getLocalTxs
    pbr <- St.processBlock localTxs slotId block
    let (globalChanged, toRollback) =
            case pbr of
                St.PBRgood (toRoll, _) -> (True, toRoll)
                _                      -> (False, 0)
    when globalChanged $ do --synchronize local data with global data
        sscApplyGlobalState =<< St.getGlobalMpcData
        txLocalDataRollback toRollback
        txApplyHeadUtxo =<< fromJust <$> St.getUtxoByDepth 0
    let blkHash = headerHash block
    case pbr of
        St.PBRabort msg -> do
            let fmt =
                    "Block " %build %
                    " processing is aborted for the following reason: " %stext
            logWarning $ sformat fmt blkHash msg
        St.PBRgood (0, (blkAdopted :| [])) -> do
            let adoptedBlkHash = headerHash blkAdopted
            jlLog $ jlAdoptedBlock blkAdopted
            logInfo $ sformat ("Received block has been adopted: " %build) adoptedBlkHash
        St.PBRgood (rollbacked, altChain) -> do
            logNotice $
                sformat
                    ("As a result of block processing, rollback" % " of " %int %
                     " blocks has been done and alternative" %
                     " chain has been adopted " %listJson)
                    rollbacked
                    (fmap headerHash altChain)
        St.PBRmore h -> do
            logInfo $
                sformat
                    ("After processing block " %build % ", we need block " %build)
                    blkHash
                    h
            replyToNode $ RequestBlock h
    whenM (ncPropagation <$> getNodeContext) $ propagateBlock pbr
    -- We assert that the chain is consistent – none of the transactions in a
    -- block are present in previous blocks. This is an expensive check and
    -- we only do it when asserts are turned on.
    inAssertMode $
        do let getTxs (Left _)    = mempty
               getTxs (Right blk) = HS.fromList . toList $ blk ^. blockTxs
           chain <- St.getBestChain
           let dups :: [(HeaderHash ssc, HashSet Tx)]
               dups = go mempty (reverse (toList chain))
                 where
                   go _txs [] = []
                   go txs (blk:blks) =
                       (headerHash blk, HS.intersection (getTxs blk) txs) :
                       go (txs <> getTxs blk) blks
           unless (all (null . snd) dups) $
               logError $
               sformat
                   ("transactions from some blocks are present in previous blocks;" %
                    " here's the whole blockchain from youngest to oldest block," %
                    " and duplicating transactions in those blocks: " %
                    listJsonIndent 2)
                   [ if null txs
                        then bprint shortHashF h
                        else bprint (shortHashF % ": " %listJsonIndent 4) h txs
                   | (h, txs) <- reverse dups ]

propagateBlock
    :: (WorkMode ssc m)
    => St.ProcessBlockRes ssc -> m ()
propagateBlock (St.PBRgood (_, blocks)) =
    either (const pass) announceBlock header
  where
    blk = NE.last blocks
    header = getBlockHeader blk
propagateBlock _ = pass

-- | Handle 'SendBlockHeader' message.
handleBlockHeader
    :: (ResponseMode ssc m)
    => SendBlockHeader ssc -> m ()
handleBlockHeader (SendBlockHeader header) =
    whenM checkUsefulness $ replyToNode (RequestBlock h)
  where
    header' = Right header
    h = hash header'
    checkUsefulness = do
        slotId <- getCurrentSlot
        verRes <- St.mayBlockBeUseful slotId header
        case verRes of
            VerFailure errors -> do
                let fmt =
                        "Ignoring header with hash "%build%
                        " for the following reasons: "%listJson
                let msg = sformat fmt h errors
                False <$ logDebug msg
            VerSuccess -> do
                let fmt = "Block header " % build % " considered useful"
                    msg = sformat fmt h
                True <$ logDebug msg

-- | Handle 'RequsetBlock' message.
handleBlockRequest
    :: (ResponseMode ssc m)
    => RequestBlock ssc -> m ()
handleBlockRequest (RequestBlock h) = do
    logDebug $ sformat ("Block " %build % " is requested") h
    maybe logNotFound sendBlockBack =<< St.getBlock h
  where
    logNotFound = logWarning $ sformat ("Block " %build % " wasn't found") h
    sendBlockBack block = do
        logDebug $ sformat ("Sending block " %build % " in reply") h
        replyToNode $ MsgBlock block

-- | Handle 'RequestBlockchainPart' message
handleBlockchainPartRequest
    :: (ResponseMode ssc m)
    => RequestBlockchainPart ssc -> m ()
handleBlockchainPartRequest RequestBlockchainPart {..} = do
    logDebug $ sformat ("Blockchain part (range "%build%".."%build%
                        ", count "%build%" is requested")
        rbFromBlock rbUntilBlock rbCount
    either logErr sendChainPart =<< St.getChainPart rbFromBlock rbUntilBlock rbCount
  where
    logErr = logWarning . sformat ("Error while fetching part of blockchain: "%stext)
    sendChainPart cp = do
        let fstH = headerHash <$> head cp
            lc = length cp
        logDebug $ sformat ("Sending chain part of length "%int%
                            ", starting with "%build) lc fstH
        replyToNode $ SendBlockchainPart cp
